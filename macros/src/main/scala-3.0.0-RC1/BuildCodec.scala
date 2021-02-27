package zd
package proto

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._
import scala.collection.immutable.ArraySeq

trait BuildCodec extends Common {
  implicit val qctx: Quotes
  import qctx.reflect.{_, given}
  import qctx.reflect.defn._
  import report._

  def prepareTrait[A: Type](a: Expr[A], params: List[FieldInfo]): Expr[Prepare] =
    val a_term = a.asTerm
    val a_tpe = TypeRepr.of[A]
    val ifBranches: List[(Term, Term)] = params.map { p =>
      val condition: Term = 
        if p.isCaseObject then Select.unique(a_term, "==").appliedTo(Ref(p.sym))
        else Select.unique(a_term, "isInstanceOf").appliedToType(p.tpe)
      val action: Term = prepareImpl(a, List(p)).asTerm
      condition -> action
    }
    val error = s"Wrong type of child of sealed trait: ${a_tpe.typeSymbol.fullName}"
    val elseBranch: Term = '{ throw new RuntimeException(${Expr(error)}) }.asTerm
    mkIfStatement(ifBranches, elseBranch).asExprOf[Prepare]

  def prepareImpl[A: Type](a: Expr[A], params: List[FieldInfo]): Expr[Prepare] =
    val sizeAccSym = Symbol.newVal(Symbol.spliceOwner, "sizeAcc", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
    val sizeAccRef = Ref(sizeAccSym)
    val sizeAccValDef = ValDef(sizeAccSym, Some(Literal(IntConstant(0))))
    val xs = params.flatMap(p => size(a, p, sizeAccRef))
    val newPrepare = '{
      new Prepare {
        val size: Int = ${ sizeAccRef.asExprOf[Int] }
        def write(os: CodedOutputStream): Unit = ${ writeImpl(a, params, 'os) }
      }
    }.asTerm
    Block(
      sizeAccValDef :: xs
    , newPrepare
    ).asExprOf[Prepare]

  def writeImpl[A: Type](a: Expr[A], params: List[FieldInfo], os: Expr[CodedOutputStream]): Expr[Unit] =
    Expr.block(
      params.flatMap(p =>
        if      p.isCaseObject then writeCaseObject(os, p)
        else if p.tpe.isCommonType then writeCommon(a, os, p)
        else if p.tpe.isOption     then writeOption(a, os, p)
        else if p.tpe.isIterable   then writeCollection(a, os, p)
        else writeMessage(a, os, p)
      )
    , unitExpr)

  def writeCommon[A: Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , writeFun(os, field.tpe, field.getter(a.asTerm))
    )
  
  def writeOption[A: Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe = field.tpe.optionArgument
    val getter = field.getter(a.asTerm)
    val getterOption = Select.unique(getter, "get")
    if tpe.isCommonType then
      List(
        '{
          if ${Select.unique(getter, "isDefined").asExprOf[Boolean]} then {
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${writeFun(os, tpe, getterOption)}
          }
        }
      )
    else
      val prepareOptionRef = Ref(field.prepareOptionSym).asExprOf[Option[Prepare]]
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

  def writeCollection[A: Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe1 = field.tpe.iterableArgument
    val getter = field.getter(a.asTerm)
    val pType = tpe1.asType
    val sizeRef = Ref(field.sizeSym)
    if tpe1.isCommonType then
      pType match
        case '[t] =>
          if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then
            val expr = 
              Select.unique(getter, "foreach")
                .appliedToType(unitLiteral.tpe)
                .appliedTo(
                  '{ (v: t) => {
                      ${os}.writeUInt32NoTag(${Expr(field.tag)})
                      ${writeFun(os, tpe1, 'v.asTerm)}
                    }
                  }.asTerm
                ).asExprOf[Unit]
              // a.field.foreach((v: V) => {
              //   os.writeUInt32NoTag(field.tag)
              //   writeFun(os, v)
              // })
            List(expr)
          else
            List(
              '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
            , '{ ${os}.writeUInt32NoTag(${sizeRef.asExprOf[Int]}) }
            , Select.unique(getter, "foreach")
                .appliedToType(unitLiteral.tpe)
                .appliedTo('{ (v: t) => ${writeFun(os, tpe1, 'v.asTerm)} }.asTerm)
                .asExprOf[Unit]
              // a.field.foreach((v: V) => writeFun(os, v))
            )
    else
      val prepareArrayRef = Ref(field.prepareArraySym).asExprOf[Array[Prepare]]
      List(
        '{
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

  def writeMessage[A: Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val prepareRef = Ref(field.prepareSym).asExprOf[Prepare]
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , '{ ${os}.writeUInt32NoTag(${prepareRef}.size) }
    , '{ ${prepareRef}.write(${os}) }
    )
  def writeCaseObject(os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , '{ ${os}.writeUInt32NoTag(0) }
    )

  def size[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    if      field.isCaseObject     then sizeCaseObject(field, sizeAcc)
    else if field.tpe.isCommonType then sizeCommon(a, field, sizeAcc)
    else if field.tpe.isOption     then sizeOption(a, field, sizeAcc)
    else if field.tpe.isIterable   then sizeCollection(a, field, sizeAcc)
    else sizeMessage(a, field, sizeAcc)

  def sizeCommon[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val fun = sizeFun(field.tpe, field.getter(a.asTerm))
    val sum = '{ ${Expr(CodedOutputStream.computeTagSize(field.num))} + ${fun} }
    List(increment(sizeAcc, sum))
  
  def sizeOption[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val tpe = field.tpe.optionArgument
    val getter: Term = field.getter(a.asTerm)
    val getterOption: Term = Select.unique(getter, "get")//getterOptionTerm(a, field)
    if (tpe.isCommonType) then
      val fun = sizeFun(tpe, getterOption)
      val sum = '{ ${Expr(CodedOutputStream.computeTagSize(field.num))} + ${fun} }
      val incrementSize = increment(sizeAcc, sum)
      val isDefined = Select.unique(getter, "isDefined")
      List(If(isDefined, incrementSize, unitLiteral))
    else
      val prepareOptionRhs = '{
        if (${Select.unique(getter, "isDefined").asExprOf[Boolean]}) {
          val p: Prepare = ${Select.unique(findCodec(tpe), "prepare").appliedTo(getterOption).asExprOf[Prepare]}
          ${
            increment(
              sizeAcc
            , '{${Expr(CodedOutputStream.computeTagSize(field.num))} + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
            ).asExpr
          }
          Some(p)
        } else None
      }
      List(
        ValDef(field.prepareOptionSym, Some(prepareOptionRhs.asTerm))
      )

  def sizeCollection[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] = 
    val tpe1 = field.tpe.iterableArgument
    val getter = field.getter(a.asTerm)
    val pType = tpe1.asType
    pType match
      case '[t] =>
        if tpe1.isCommonType then
          val sizeRef = Ref(field.sizeSym)
          val sizeValDef = ValDef(field.sizeSym, Some(Literal(IntConstant(0))))
          if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then
            val sizeExpr = '{ 
              val tagSize = ${Expr(CodedOutputStream.computeTagSize(field.num))}
              ${
                Select.unique(getter, "foreach")
                  .appliedToType(unitLiteral.tpe)
                  .appliedTo(
                   '{ (v: t) => 
                      ${ increment(sizeRef, '{ ${sizeFun(tpe1, 'v.asTerm)} + tagSize }).asExprOf[Unit] } 
                    }.asTerm
                  ).asExpr
              } // a.field.foreach((v: V) =>  sizeRef = sizeRef + sizeFun(v) + tagSize)
            }
            val incrementAcc = increment(sizeAcc, sizeRef.asExprOf[Int])
            List(sizeValDef, sizeExpr.asTerm, incrementAcc)
          else
            // a.field.foreach((v: V) =>  sizeRef = sizeRef + sizeFun(v))
            val sizeExpr = 
              Select.unique(getter, "foreach")
                .appliedToType(unitLiteral.tpe)
                .appliedTo(
                 '{ (v: t) => 
                    ${ increment(sizeRef, sizeFun(tpe1, 'v.asTerm)).asExprOf[Unit] } 
                  }.asTerm
                )
            val sizeRefExpr = sizeRef.asExprOf[Int]
            val sum = '{ 
              ${Expr(CodedOutputStream.computeTagSize(field.num))} + 
              CodedOutputStream.computeUInt32SizeNoTag(${sizeRefExpr}) +
              ${sizeRefExpr}
            }
            val incrementAcc = increment(sizeAcc, sum)
            List(sizeValDef, sizeExpr, incrementAcc)
        else
          val prepareArrayRef = Ref(field.prepareArraySym)
          val prepareArrayRhs = '{ new Array[Prepare](${Select.unique(getter, "size").asExprOf[Int]}) }
          val sizeExpr = '{
            var counter = 0
            ${ Select.unique(getter, "foreach")
                  .appliedToType(unitLiteral.tpe)
                  .appliedTo(
                   '{ (v: t) =>
                      val p: Prepare = ${Select.unique(findCodec(tpe1), "prepare").appliedTo('v.asTerm).asExprOf[Prepare]}
                      ${prepareArrayRef.asExprOf[Array[Prepare]]}(counter) = p
                      ${
                        increment(
                          sizeAcc
                        , '{${Expr(CodedOutputStream.computeTagSize(field.num))} + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
                        ).asExpr
                      }
                      counter = counter + 1                    
                    }.asTerm
                  ).asExpr 
                  // a.field.foreach((v: V) => {
                  //   val p: Prepare = implicitly[MessageCodec[V]].prepare(v)
                  //   prepareArra[counter] = p
                  //   sizeAcc = sizeAcc + CodedOutputStream.computeTagSize(field.num) + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size
                  //   counter = counter + 1
                  // })
            }
          }
          List(
            ValDef(field.prepareArraySym, Some(prepareArrayRhs.asTerm))
          , sizeExpr.asTerm
          )

  def sizeMessage[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val getter = field.getter(a.asTerm)
    val prepare = Select.unique(findCodec(field.tpe), "prepare").appliedTo(getter)
    val prepareValDef = ValDef(field.prepareSym, Some(prepare))
    val prepareRef = Ref(field.prepareSym).asExprOf[Prepare]
    val sum = '{ 
      ${Expr(CodedOutputStream.computeTagSize(field.num))} + 
      CodedOutputStream.computeUInt32SizeNoTag(${prepareRef}.size) +
      ${prepareRef}.size
    }
    val incrementAcc = increment(sizeAcc, sum)
    List(prepareValDef, incrementAcc)
  
  def sizeCaseObject(field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val sum = '{ 
      ${ Expr(CodedOutputStream.computeTagSize(field.num)) } + 
      ${ Expr(CodedOutputStream.computeUInt32SizeNoTag(0)) }
    }
    List(increment(sizeAcc, sum))

  def readImpl(t: TypeRepr, params: List[FieldInfo], is: Expr[CodedInputStream], isTrait: Boolean=false, constructor: Option[Term]=None): Expr[Any] = {

    val (initStatements, readRefs, resExp): (List[Statement], List[Term], Term) =
      if isTrait then
      // if t.isSealedTrait then
        val _none = Ref(NoneModule)
        val sym = Symbol.newVal(Symbol.spliceOwner, "readRes", OptionType.appliedTo(t), Flags.Mutable, Symbol.noSymbol)
        val init = ValDef(sym, Some(_none))
        val ref = Ref(sym)
        val error = s"missing one of required field for ${t.typeSymbol.fullName}"
        val exception = '{ throw new RuntimeException(${Expr(error)}) }.asTerm
        val res = Select.unique(ref, "getOrElse")
          .appliedToType(t)
          .appliedTo(exception) // ref.getOrElse[t](exception)
        (List(init), List.fill(params.size)(ref), res)
      else
        val xs = params.map(p => {
          val (init, ref) = initValDef(p)
          val res = resTerm(ref, p)
          (init, ref, res)
        }).unzip3
        val res = classApply(t, xs._3, constructor)
        (xs._1, xs._2, res)

    val tagMatch: Statement = '{
      var done = false
      while (done == false) {
        val tag: Int = ${is}.readTag
        ${
          val ifBranches: List[(Term, Term)] =
            ('{ tag == 0 }.asTerm -> '{ done = true; }.asTerm) ::
            params.zip(readRefs).map{ case (p, ref) =>
              val paramTag = Expr(p.tag)
              '{ tag == ${paramTag} }.asTerm -> readContentImpl(p, ref, is).asTerm
            }
          val elseBranch: Term = '{ ${is}.skipField(tag) }.asTerm
          mkIfStatement(ifBranches, elseBranch).asExprOf[Any]
        }
      }
    }.asTerm

    val statements =
      if (params.size > 0) then initStatements :+ (tagMatch) 
      else Nil
    
    Block(
      statements
    , resExp
    ).asExpr
  }

  def readContentImpl(p: FieldInfo, readRef: Term, is: Expr[CodedInputStream]): Expr[Any] =
    if p.isCaseObject then
      val fun: Term = Ref(p.sym)
      putLimit(
        is
      , Assign(
          readRef
        , Some_Apply(tpe=p.tpe, value=fun)
        ).asExpr 
      )
    else if p.tpe.isCommonType then
      val fun: Term = readFun(p.tpe, is)
      Assign(
        readRef
      , Some_Apply(tpe=p.tpe, value=fun)
      ).asExpr
    else if p.tpe.isOption && p.tpe.optionArgument.isCommonType then
      val tpe1 = p.tpe.optionArgument
      val fun: Term = readFun(tpe1, is)
      Assign(
        readRef
      , Some_Apply(tpe=tpe1, value=fun)
      ).asExpr
    else if p.tpe.isOption then
      val tpe1 = p.tpe.optionArgument
      val fun: Term = Select.unique(findCodec(tpe1), "read").appliedTo(is.asTerm)
      putLimit(
        is
      , Assign(
          readRef
        , Some_Apply(tpe=tpe1, value=fun)
        ).asExpr      
      )
    else if p.tpe.isIterable && p.tpe.iterableArgument.isCommonType then
      val tpe1 = p.tpe.iterableArgument
      val fun: Term = readFun(tpe1, is)
      val addOneApply = Select.unique(readRef, "addOne").appliedTo(fun).asExpr
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then 
        addOneApply
      else
        putLimit(
          is
        , '{ while (${is}.getBytesUntilLimit > 0) ${addOneApply} }
        )
    else if p.tpe.isIterable then
      val tpe1 = p.tpe.iterableArgument
      val fun: Term = Select.unique(findCodec(tpe1), "read").appliedTo(is.asTerm)
      val addOneApply = Select.unique(readRef, "addOne").appliedTo(fun).asExpr
      putLimit(
        is
      , addOneApply
      )
    else
      val fun: Term = Select.unique(findCodec(p.tpe), "read").appliedTo(is.asTerm)
      putLimit(
        is
      , Assign(
          readRef
        , Some_Apply(tpe=p.tpe, value=fun)
        ).asExpr 
      )

  def putLimit(is: Expr[CodedInputStream], read: Expr[Any]): Expr[Unit] =
    '{
      val readSize: Int = ${is}.readRawVarint32
      val limit = ${is}.pushLimit(readSize)
      ${read}
      ${is}.popLimit(limit)
    }

  def initValDef(field: FieldInfo): (ValDef, Ref) =
    if field.tpe.isOption then
      val _none = Ref(NoneModule)
      val sym = Symbol.newVal(Symbol.spliceOwner, s"${field.name}Read", field.tpe, Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym)
      init -> ref
    else if field.tpe.isIterable then
      val tpe1 = field.tpe.iterableArgument
      val collectionType = field.tpe.iterableBaseType
      val collectionCompanion = collectionType.typeSymbol.companionModule
      val builderTpe = builderType.appliedTo(List(tpe1, field.tpe))
      val sym = Symbol.newVal(Symbol.spliceOwner, s"${field.name}Read", builderTpe, Flags.EmptyFlags, Symbol.noSymbol)
      val rhs = Select.unique(Ref(collectionCompanion), "newBuilder")
      val init = ValDef(sym, Some(rhs))
      val ref = Ref(sym)
      init -> ref
    else
      val _none = Ref(NoneModule)
      val sym = Symbol.newVal(Symbol.spliceOwner, s"${field.name}Read", OptionType.appliedTo(field.tpe), Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym)
      init -> ref

  def resTerm(ref: Ref, field: FieldInfo): Term =
    if field.tpe.isOption then ref
    else if field.tpe.isIterable then
      Select.unique(ref, "result")
    else
      val error = s"missing required field `${field.name}: ${field.tpe.typeSymbol.name}`"
      val exception = '{ throw new RuntimeException(${Expr(error)}) }.asTerm
      val orElse = field.defaultValue.getOrElse(exception)
      Select.unique(ref, "getOrElse")
        .appliedToType(field.tpe)
        .appliedTo(orElse) // ref.getOrElse[feld.tpe](orElse)
  
  def findCodec(t: TypeRepr): Term = 
    val tpe = TypeRepr.of[MessageCodec].appliedTo(t)
    Implicits.search(tpe) match
      case x: ImplicitSearchSuccess => x.tree
      case _: ImplicitSearchFailure => throwError(s"could not find implicit codec for `${t.typeSymbol.fullName}`")

  def classApply(t: TypeRepr, params: List[Term], constructor: Option[Term]): Term =
    constructor match
      case Some(fun) =>
        Select.unique(fun, "apply")
          .appliedToArgs(params)
      case None => 
        t match
          case x: TermRef => Ident(x)
          case x: TypeRef =>
            val companion = x.typeSymbol.companionModule
            Select.unique(Ref(companion) , "apply")
              .appliedToArgs(params)
          case x: AppliedType =>
            val companion = x.typeSymbol.companionModule
            Select.unique(Ref(companion) , "apply")
              .appliedToTypes(x.typeArgs)
              .appliedToArgs(params)

  def increment(x: Ref, y: Expr[Int]): Assign =  Assign(x, '{ ${x.asExprOf[Int]} + ${y} }.asTerm)
}