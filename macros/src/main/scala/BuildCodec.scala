package zd
package proto

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._
import scala.quoted.matching._
import scala.internal.quoted.showName
import scala.collection.immutable.ArraySeq
import zd.proto.Bytes

trait BuildCodec extends Common {
  implicit val qctx: QuoteContext
  import qctx.tasty.{_, given _}
  import qctx.tasty.defn._

  def prepareImpl[A: quoted.Type](a: Expr[A], params: List[FieldInfo])(using ctx: Context): Expr[Prepare] =
    val sizeAccSym = Symbol.newVal(ctx.owner, "sizeAcc", IntType, Flags.Mutable, Symbol.noSymbol)
    val sizeAccRef = Ref(sizeAccSym)
    val sizeAccValDef = ValDef(sizeAccSym, Some(Literal(Constant(0))))
    val xs = params.flatMap(p => size(a, p, sizeAccRef))
    val newPrepare = '{
      new Prepare {
        val size: Int = ${ sizeAccRef.seal.cast[Int] }
        def write(os: CodedOutputStream): Unit = ${ writeImpl(a, params, 'os) }
      }
    }.unseal
    Block(
      sizeAccValDef :: xs
    , newPrepare
    ).seal.cast[Prepare]

  def writeImpl[A: quoted.Type](a: Expr[A], params: List[FieldInfo], os: Expr[CodedOutputStream]): Expr[Unit] =
    Expr.block(
      params.flatMap(p =>
        if p.tpe.isCommonType then writeCommon(a, os, p)
        else if p.tpe.isOption then writeOption(a, os, p)
        else if p.tpe.isIterable then writeCollection(a, os, p)
        else writeMessage(a, os, p)
      )
    , Expr.unitExpr)

  def writeCommon[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , writeFun(os, field.tpe, getterTerm(a, field))
    )
  
  def writeOption[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe = field.tpe.optionArgument
    val getter = getterTerm(a, field)
    val getterOption = getterOptionTerm(a, field)
    if tpe.isCommonType then
      List(
        '{
          if ${getter.seal.cast[Option[Any]]}.isDefined then {
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${writeFun(os, tpe, getterOption)}
          }
        }
      )
    else
      val prepareOptionRef = Ref(field.prepareOptionSym).seal.cast[Option[Prepare]]
      List(
        '{
          if ${prepareOptionRef}.isDefined then {
            val p = ${prepareOptionRef}.get
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${os}.writeUInt32NoTag(p.size)
            p.write(${os})
          }
        }
      )

  def writeCollection[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe1 = field.tpe.iterableArgument
    val getter = getterTerm(a, field).seal.cast[Iterable[Any]]
    val pType = tpe1.seal.asInstanceOf[quoted.Type[Any]]
    val sizeRef = Ref(field.sizeSym)
    if tpe1.isCommonType then
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then
        val expr = '{
          ${getter}.foreach((v: ${pType}) => {
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${writeFun(os, tpe1, 'v.unseal)}
          })
        }
        List(expr)
      else
        List(
          '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
        , '{ ${os}.writeUInt32NoTag(${sizeRef.seal.cast[Int]}) }
        , '{ ${getter}.foreach((v: ${pType}) => ${writeFun(os, tpe1, 'v.unseal)} ) }
        )
    else
      val prepareArrayRef = Ref(field.prepareArraySym).seal.cast[Array[Prepare]]
      val counterName = s"${field.name}Counter"
      List(
        '{
          @showName(${Expr(counterName)})
          var counter = 0
          while (counter < ${prepareArrayRef}.length) {
            val p = ${prepareArrayRef}(counter)
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${os}.writeUInt32NoTag(p.size)
            p.write(${os}) 
            counter = counter + 1
          }
        }
      )

  def writeMessage[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val prepareRef = Ref(field.prepareSym).seal.cast[Prepare]
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , '{ ${os}.writeUInt32NoTag(${prepareRef}.size) }
    , '{ ${prepareRef}.write(${os}) }
    )

  def size[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
   if field.tpe.isCommonType then sizeCommon(a, field, sizeAcc)
   else if field.tpe.isOption then sizeOption(a, field, sizeAcc)
   else if field.tpe.isIterable then sizeCollection(a, field, sizeAcc)
   else sizeMessage(a, field, sizeAcc)

  def sizeCommon[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val fun = sizeFun(field.tpe, getterTerm(a, field))
    val sum = '{ CodedOutputStream.computeTagSize(${Expr(field.num)}) + ${fun} }
    List(increment(sizeAcc, sum))
  
  def sizeOption[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val tpe = field.tpe.optionArgument
    val getter = getterTerm(a, field)
    val getterOption = getterOptionTerm(a, field)
    if (tpe.isCommonType) then
      val fun = sizeFun(tpe, getterOption)
      val sum = '{ CodedOutputStream.computeTagSize(${Expr(field.num)}) + ${fun} }
      val incrementSize = increment(sizeAcc, sum)
      val isDefined = Select(getter, OptionClass.method("isDefined").head)
      List(If(isDefined, incrementSize, unitLiteral))
    else
      val prepareOptionRhs = '{
        if (${getter.seal.cast[Option[Any]]}.isDefined) {
          val p: Prepare = ${findCodec(tpe)}.prepare(${getterOption.seal})
          ${
            increment(
              sizeAcc
            , '{CodedOutputStream.computeTagSize(${Expr(field.num)}) + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
            ).seal
          }
          Some(p)
        } else None
      }
      List(
        ValDef(field.prepareOptionSym, Some(prepareOptionRhs.unseal))
      )

  def sizeCollection[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] = 
    val tpe1 = field.tpe.iterableArgument
    val getter = getterTerm(a, field).seal.cast[Iterable[Any]]
    val pType = tpe1.seal.asInstanceOf[quoted.Type[Any]]
    if tpe1.isCommonType then
      val sizeRef = Ref(field.sizeSym)
      val sizeValDef = ValDef(field.sizeSym, Some(Literal(Constant(0))))
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then
        val tagSizeName = s"${field.name}TagSize"
        val sizeExpr = '{ 
          @showName(${Expr(tagSizeName)})
          val tagSize = CodedOutputStream.computeTagSize(${Expr(field.num)})
          ${getter}.foreach((v: ${pType}) => ${ increment(sizeRef, '{ ${sizeFun(tpe1, 'v.unseal)} + tagSize }).seal }  )
        }
        val incrementAcc = increment(sizeAcc, sizeRef.seal.cast[Int])
        List(sizeValDef, sizeExpr.unseal, incrementAcc)
      else
        val sizeExpr = '{
          ${getter}.foreach((v: ${pType}) => ${ increment(sizeRef, sizeFun(tpe1, 'v.unseal)).seal } )
        }
        val sizeRefExpr = sizeRef.seal.cast[Int]
        val sum = '{ 
          CodedOutputStream.computeTagSize(${Expr(field.num)}) + 
          CodedOutputStream.computeUInt32SizeNoTag(${sizeRefExpr}) +
          ${sizeRefExpr}
        }
        val incrementAcc = increment(sizeAcc, sum)
        List(sizeValDef, sizeExpr.unseal, incrementAcc)
    else
      val prepareArrayRef = Ref(field.prepareArraySym)
      val prepareArrayRhs = '{ new Array[Prepare](${getter}.size) }
      val counterName = s"${field.name}Counter"
      val sizeExpr = '{
        @showName(${Expr(counterName)})
        var counter = 0
        ${getter}.foreach((v: ${pType}) => {
          val p: Prepare = ${findCodec(tpe1)}.prepare(v)
          ${prepareArrayRef.seal.cast[Array[Prepare]]}(counter) = p
          ${
            increment(
              sizeAcc
            , '{CodedOutputStream.computeTagSize(${Expr(field.num)}) + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
            ).seal
          }
          counter = counter + 1
        })
      }
      List(
        ValDef(field.prepareArraySym, Some(prepareArrayRhs.unseal))
      , sizeExpr.unseal
      )

  def sizeMessage[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val getter = getterTerm(a, field).seal
    val prepare = '{ ${findCodec(field.tpe)}.prepare(${getter}) }
    val prepareValDef = ValDef(field.prepareSym, Some(prepare.unseal))
    val prepareRef = Ref(field.prepareSym).seal.cast[Prepare]
    val sum = '{ 
      CodedOutputStream.computeTagSize(${Expr(field.num)}) + 
      CodedOutputStream.computeUInt32SizeNoTag(${prepareRef}.size) +
      ${prepareRef}.size
    }
    val incrementAcc = increment(sizeAcc, sum)
    List(prepareValDef, incrementAcc)

  def getterTerm[A: quoted.Type](a: Expr[A], field: FieldInfo): Term =
    Select(a.unseal, field.getter)

  def getterOptionTerm[A: quoted.Type](a: Expr[A], field: FieldInfo): Term =
    Select(Select(a.unseal, field.getter), OptionClass.method("get").head)

  def readImpl(t: Type, params: List[FieldInfo], is: Expr[CodedInputStream])(using ctx: Context): Expr[Any] = {

    if (params.size > 0) {
      val xs: List[(Statement, Term, Term)] = params.map(p => {
        val (init, ref) = initValDef(p)
        val res = resTerm(ref, p)
        (init, ref, res)
      })

      val read: Statement = '{
        var done = false;
        while (done == false) {
          val tag: Int = ${is}.readTag
          var tagMatch: Boolean = false
          if (tag == 0) { done = true; tagMatch = true }
          ${
            Expr.block(params.zip(xs).map{ case (p, (_,ref,_)) => {
              val paramTag = Expr(p.tag)
              val readContent = readContentImpl(p, ref, is)
              '{  if (tag == ${paramTag}) { 
                    tagMatch = true
                    ${readContent}
                  }
              }
            }}, Expr.unitExpr)
          }
          if (tagMatch == false) ${is}.skipField(tag)
        }
      }.unseal

      val statements = xs.map(_._1) :+ (read)
      val resTerms = xs.map(_._3)
      Block(
        statements
      , classApply(t, resTerms)
      ).seal
    } else {
      classApply(t, Nil).seal
    }
  }

  def readContentImpl(p: FieldInfo, readRef: Term, is: Expr[CodedInputStream]): Expr[Any] =
    if (p.tpe.isCommonType) then
      val fun = readFun(p.tpe, is)
      Assign(readRef, '{Some(${fun})}.unseal).seal
    else if p.tpe.isOption && p.tpe.optionArgument.isCommonType then
      val fun = readFun(p.tpe.optionArgument, is)
      Assign(readRef, '{Some(${fun})}.unseal).seal
    else if p.tpe.isOption then
      putLimit(
        is
      , Assign(readRef, '{Some(${findCodec(p.tpe.optionArgument)}.read(${is}))}.unseal).seal 
      )
    else if p.tpe.isIterable && p.tpe.iterableArgument.isCommonType then
      val tpe1 = p.tpe.iterableArgument
      val fun = readFun(tpe1, is)
      val addOneApply = Apply(
        Select(readRef, readRef.tpe.termSymbol.method("addOne").head)
      , List(fun.unseal)
      ).seal
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then 
        addOneApply
      else
        putLimit(
          is
        , '{ while (${is}.getBytesUntilLimit > 0) ${addOneApply} }
        )
    else if p.tpe.isIterable then
      val tpe1 = p.tpe.iterableArgument
      val fun = '{ ${findCodec(tpe1)}.read(${is}) }
      val addOneApply = Apply(
        Select(readRef, readRef.tpe.termSymbol.method("addOne").head)
      , List(fun.unseal)
      ).seal
      putLimit(
        is
      , addOneApply
      )
    else
      putLimit(
        is
      , Assign(readRef, '{Some(${findCodec(p.tpe)}.read(${is}))}.unseal).seal 
      )

  def putLimit(is: Expr[CodedInputStream], read: Expr[Any]): Expr[Unit] =
    '{
      val readSize: Int = ${is}.readRawVarint32
      val limit = ${is}.pushLimit(readSize)
      ${read}
      ${is}.popLimit(limit)
    }

  def initValDef(field: FieldInfo)(using ctx: Context): (ValDef, Ident) =
    if field.tpe.isOption then
      // val pType = field.tpe.seal.asInstanceOf[quoted.Type[Any]]
      // val _none = '{ None:${pType} }.unseal
      val _none = Typed(Ref(NoneModule), field.tpt)
      val sym = Symbol.newVal(ctx.owner, s"${field.name}Read", field.tpe, Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym).asInstanceOf[Ident]
      init -> ref
    else if field.tpe.isIterable then
      val tpe1 = field.tpe.iterableArgument
      val collectionType = field.tpe.iterableBaseType
      val collectionCompanion = collectionType.typeSymbol.companionModule
      val newBuilderMethod = collectionCompanion.method("newBuilder").head
      val builderType = appliedBuilderType(tpe1, field.tpe)
      val sym = Symbol.newVal(ctx.owner, s"${field.name}Read", builderType, Flags.EmptyFlags, Symbol.noSymbol)
      val rhs = Select(Ref(collectionCompanion), newBuilderMethod)
      val init = ValDef(sym, Some(rhs))
      val ref = Ref(sym).asInstanceOf[Ident]
      init -> ref
    else
      val pType = field.tpe.seal.asInstanceOf[quoted.Type[Any]]
      val _none = '{ None:Option[${pType}] }.unseal
      val sym = Symbol.newVal(ctx.owner, s"${field.name}Read", _none.tpe, Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym).asInstanceOf[Ident]
      init -> ref

  def resTerm(ref: Ident, field: FieldInfo): Term =
    if field.tpe.isOption then ref
    else if field.tpe.isIterable then
      Select(ref, ref.tpe.termSymbol.method("result").head)
    else
      val error = s"missing required field `${field.name}: ${field.tpe.typeSymbol.name}`"
      val exception = '{ throw new RuntimeException(${Expr(error)}) }.unseal
      val orElse = field.defaultValue.getOrElse(exception)
      Apply(
        TypeApply(
          Select(ref, OptionClass.method("getOrElse").head)
        , List(field.tpt)
        )
      , List(orElse)
      )

  def findCodec(t: Type): Expr[MessageCodec[Any]] = 
    val tpe = t.seal.asInstanceOf[quoted.Type[Any]]
    val msgCodecTpe = '[MessageCodec[$tpe]]
    Expr.summon(using msgCodecTpe) match
      case Some(expr) => expr
      case None => qctx.throwError(s"could not find implicit codec for `${tpe.show}`")

  def classApply(t: Type, params: List[Term]): Term =
    t match
      case y: TermRef => Ident(y)
      case x @ TypeRef(_) =>
        val sym = x.typeSymbol
        val applyMethod = sym.companionModule.method("apply").head
        Apply(Select(Ref(sym.companionModule), applyMethod), params)

  def increment(x: Ref, y: Expr[Int]): Assign =  Assign(x, '{ ${x.seal.cast[Int]} + ${y} }.unseal)
}