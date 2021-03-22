package purs

import scala.quoted.*
import proto.*
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.collection.immutable.ArraySeq

inline def enumByN[A, B]: String = ${enumByN[A, B]}

def enumByN[A: Type, B: Type](using qctx: Quotes): Expr[String] = Impl().enumByN[A, B]

private class Impl(using qctx: Quotes):
  import qctx.reflect.{*, given}
  import qctx.reflect.defn.*
  import report.*
  
  extension (x: TypeRepr)
    private def matchable: TypeRepr & Matchable = x.asInstanceOf[TypeRepr & Matchable]

  def enumByN[A: Type, B: Type]: Expr[String] =
    val a_tpe = TypeRepr.of[A]
    val b_tpe = TypeRepr.of[B]
    val a_fields = fieldsOf(a_tpe.matchable)
    val b_fields = fieldsOf(b_tpe.matchable)
    val pushTypes =
      a_fields
      .map(_.name)
      .mkString(" | ")
    val pullTypes =
      b_fields
      .map(_.name)
      .mkString(" | ")
    val pushCases =
      a_fields
      .map(x => s"${x.num} -> decode (decode${x.name} _xs_ pos1) \\_ -> ${x.name}")
      .mkString("\n    ")
    val pullCases =
      b_fields
      .map(x => s"encodePull ${x.name} = concatAll [ Encode.unsignedVarint32 ${(x.num << 3) + 2}, encode${x.name} ]")
      .mkString("\n")
    val pushFuns =
      a_fields
      .map(x => s"""
        |decode${x.name} :: Uint8Array -> Int -> Decode.Result Unit
        |decode${x.name} _xs_ pos0 = do
        |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
        |  pure { pos: pos + msglen, val: unit }
        |""".stripMargin)
      .mkString
    val pullFuns =
      b_fields
      .map(x => s"""
        |encode${x.name} :: Uint8Array
        |encode${x.name} = Encode.unsignedVarint32 0
        |""".stripMargin)
      .mkString
    Expr(
      s"""module API
        |  ( Push(..)
        |  , decodePush
        |  , Pull(..)
        |  , encodePull
        |  ) where
        |
        |import Control.Monad.Rec.Class (Step(Loop, Done))
        |import Data.Either (Either(Left))
        |import Data.Eq (class Eq)
        |import Data.Int.Bits (zshr)
        |import Data.Unit (Unit, unit)
        |import Prelude (map, bind, pure, ($$), (+))
        |import Proto.Decode as Decode
        |import Proto.Encode as Encode
        |import Proto.Uint8Array (Uint8Array, concatAll)
        |
        |decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
        |decodeFieldLoop end res f = map (\\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res
        |
        |data Push = $pushTypes
        |derive instance eqPush :: Eq Push
        |
        |decodePush :: Uint8Array -> Decode.Result Push
        |decodePush _xs_ = do
        |  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
        |  case tag `zshr` 3 of
        |    $pushCases
        |    i -> Left $$ Decode.BadType i
        |  where
        |  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
        |  decode res f = map (\\{ pos, val } -> { pos, val: f val }) res
        |$pushFuns
        |data Pull = $pullTypes
        |derive instance eqPull :: Eq Pull
        |
        |encodePull :: Pull -> Uint8Array
        |$pullCases
        |$pullFuns""".stripMargin
    )

  private def fieldsOf(
    _tpe: TypeRepr & Matchable
  ): List[FieldInfo] =
    val _typeSymbol = _tpe.typeSymbol
    val _typeName = _typeSymbol.fullName
    val _subclasses = _typeSymbol.children

    val _nums =
      _subclasses.map{ x =>
        x.annotations.collect{
          case Apply(Select(New(tpt),_), List(Literal(IntConstant(num))))
            if tpt.tpe.matchable.isNType =>
              x.tpe -> num
        } match
          case List(x) => x
          case Nil =>
            throwError(s"missing ${NTpe.typeSymbol.name} annotation for `$_typeName`")
          case _ =>
            throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `$_typeName`")
      }

    if _subclasses.size <= 0
      then throwError(s"required at least 1 subclass for `${_typeName}`")
    if _nums.size != _subclasses.size
      then throwError(s"`${_typeName}` _subclasses ${_subclasses.size} count != _nums definition ${_nums.size}")
    if _nums.exists(_._2 < 1)
      then throwError(s"_nums for ${_typeName} should be > 0")
    if _nums.groupBy(_._2).exists(_._2.size != 1)
      then throwError(s"_nums for ${_typeName} should be unique")

    _subclasses.map{ s =>
      val tpe = s.tpe.matchable
      val num: Int = _nums.collectFirst{ case (tpe1, num) if tpe =:= tpe1 => num }.getOrElse(throwError(s"missing num for class `${tpe}` of trait `${_tpe}`"))
      if _tpe.restrictedNums.contains(num)
        then throwError(s"num ${num} is restricted for class `${tpe}` of trait `${_tpe}`")
    
      FieldInfo(
        name = s.name
      , num = num
      , sym = s
      , tpe = tpe
      , getter = 
          if s.isTerm then (a: Term) => Ref(s)
          else (a: Term) => Select.unique(a, "asInstanceOf").appliedToType(tpe)
      , sizeSym = Symbol.newVal(Symbol.spliceOwner, s"field${num}Size", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(Symbol.spliceOwner, s"field${num}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , defaultValue = None
      , isCaseObject = s.isTerm
      )
    }

  private def prepareTrait[A: Type](a: Expr[A], params: List[FieldInfo]): Expr[Prepare] =
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

  private def prepareImpl[A: Type](a: Expr[A], params: List[FieldInfo]): Expr[Prepare] =
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

  private def writeImpl[A: Type](a: Expr[A], params: List[FieldInfo], os: Expr[CodedOutputStream]): Expr[Unit] =
    Expr.block(
      params.flatMap(p =>
        if      p.isCaseObject then writeCaseObject(os, p)
        else if p.tpe.isCommonType then writeCommon(a, os, p)
        else if p.tpe.isOption     then writeOption(a, os, p)
        else if p.tpe.isIterable   then writeCollection(a, os, p)
        else writeMessage(a, os, p)
      )
    , unitExpr)

  private def writeCommon[A: Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , writeFun(os, field.tpe, field.getter(a.asTerm))
    )
  
  private def writeOption[A: Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe = field.tpe.optionArgument.matchable
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

  private def writeCollection[A: Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe1 = field.tpe.iterableArgument.matchable
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
          while counter < ${prepareArrayRef}.length do {
            val p = ${prepareArrayRef}(counter)
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${os}.writeUInt32NoTag(p.size)
            p.write(${os}) 
            counter = counter + 1
          }
        }
      )

  private def writeMessage[A: Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val prepareRef = Ref(field.prepareSym).asExprOf[Prepare]
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , '{ ${os}.writeUInt32NoTag(${prepareRef}.size) }
    , '{ ${prepareRef}.write(${os}) }
    )
  private def writeCaseObject(os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , '{ ${os}.writeUInt32NoTag(0) }
    )

  private def size[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    if      field.isCaseObject     then sizeCaseObject(field, sizeAcc)
    else if field.tpe.isCommonType then sizeCommon(a, field, sizeAcc)
    else if field.tpe.isOption     then sizeOption(a, field, sizeAcc)
    else if field.tpe.isIterable   then sizeCollection(a, field, sizeAcc)
    else sizeMessage(a, field, sizeAcc)

  private def sizeCommon[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val fun = sizeFun(field.tpe, field.getter(a.asTerm))
    val sum = '{ ${Expr(CodedOutputStream.computeTagSize(field.num))} + ${fun} }
    List(increment(sizeAcc, sum))
  
  private def sizeOption[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val tpe = field.tpe.optionArgument.matchable
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
        if ${Select.unique(getter, "isDefined").asExprOf[Boolean]} then
          val p: Prepare = ${Select.unique(findCodec(tpe), "prepare").appliedTo(getterOption).asExprOf[Prepare]}
          ${
            increment(
              sizeAcc
            , '{${Expr(CodedOutputStream.computeTagSize(field.num))} + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
            ).asExpr
          }
          Some(p)
        else None
      }
      List(
        ValDef(field.prepareOptionSym, Some(prepareOptionRhs.asTerm))
      )

  private def sizeCollection[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] = 
    val tpe1 = field.tpe.iterableArgument.matchable
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

  private def sizeMessage[A: Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
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
  
  private def sizeCaseObject(field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val sum = '{ 
      ${ Expr(CodedOutputStream.computeTagSize(field.num)) } + 
      ${ Expr(CodedOutputStream.computeUInt32SizeNoTag(0)) }
    }
    List(increment(sizeAcc, sum))

  private def readImpl(t: TypeRepr, params: List[FieldInfo], is: Expr[CodedInputStream], isTrait: Boolean=false, constructor: Option[Term]=None): Expr[Any] =

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
      while done == false do
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
    }.asTerm

    val statements =
      if (params.size > 0) then initStatements :+ (tagMatch) 
      else Nil
    
    Block(
      statements
    , resExp
    ).asExpr

  private def readContentImpl(p: FieldInfo, readRef: Term, is: Expr[CodedInputStream]): Expr[Any] =
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
    else if p.tpe.isOption && p.tpe.optionArgument.matchable.isCommonType then
      val tpe1 = p.tpe.optionArgument
      val fun: Term = readFun(tpe1.matchable, is)
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
    else if p.tpe.isIterable && p.tpe.iterableArgument.matchable.isCommonType then
      val tpe1 = p.tpe.iterableArgument.matchable
      val fun: Term = readFun(tpe1, is)
      val addOneApply = Select.unique(readRef, "addOne").appliedTo(fun).asExpr
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then 
        addOneApply
      else
        putLimit(
          is
        , '{ while ${is}.getBytesUntilLimit > 0 do ${addOneApply} }
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

  private def putLimit(is: Expr[CodedInputStream], read: Expr[Any]): Expr[Unit] =
    '{
      val readSize: Int = ${is}.readRawVarint32
      val limit = ${is}.pushLimit(readSize)
      ${read}
      ${is}.popLimit(limit)
    }

  private def initValDef(field: FieldInfo): (ValDef, Ref) =
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

  private def resTerm(ref: Ref, field: FieldInfo): Term =
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
  
  private def findCodec(t: TypeRepr): Term = 
    val tpe = TypeRepr.of[MessageCodec].appliedTo(t)
    Implicits.search(tpe) match
      case x: ImplicitSearchSuccess => x.tree
      case _: ImplicitSearchFailure => throwError(s"could not find implicit codec for `${t.typeSymbol.fullName}`")

  private def classApply(t: TypeRepr, params: List[Term], constructor: Option[Term]): Term =
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
              .appliedToTypes(x.matchable.typeArgs)
              .appliedToArgs(params)

  private def increment(x: Ref, y: Expr[Int]): Assign =  Assign(x, '{ ${x.asExprOf[Int]} + ${y} }.asTerm)

  private case class FieldInfo(
    name: String
  , num: Int
  , sym: Symbol
  , tpe: TypeRepr & Matchable
  , getter: Term => Term
  , sizeSym: Symbol
  , prepareSym: Symbol
  , prepareOptionSym: Symbol = Symbol.noSymbol
  , prepareArraySym: Symbol = Symbol.noSymbol
  , defaultValue: Option[Term]
  , isCaseObject: Boolean = false
  ):
    def tag: Int = num << 3 | wireType(tpe)

  private def wireType(t: TypeRepr & Matchable): Int =
    if      t.isInt || t.isLong || t.isBoolean then 0
    else if t.isDouble then 1
    else if t.isFloat then 5
    else if t.isOption then wireType(t.optionArgument.matchable)
    else if t.isString || 
            t.isArrayByte || 
            t.isArraySeqByte || 
            t.isBytesType || 
            t.isCaseType ||
            t.isSealedTrait ||
            t.isIterable then 2
    else 2

  private def writeFun(os: Expr[CodedOutputStream], t: TypeRepr & Matchable, getterTerm: Term): Expr[Unit] =
    if      t.isInt then '{ ${os}.writeInt32NoTag(${getterTerm.asExprOf[Int]}) }
    else if t.isLong then '{ ${os}.writeInt64NoTag(${getterTerm.asExprOf[Long]}) }
    else if t.isBoolean then '{ ${os}.writeBoolNoTag(${getterTerm.asExprOf[Boolean]}) }
    else if t.isDouble then '{ ${os}.writeDoubleNoTag(${getterTerm.asExprOf[Double]}) }
    else if t.isFloat then '{ ${os}.writeFloatNoTag(${getterTerm.asExprOf[Float]}) }
    else if t.isString then '{ ${os}.writeStringNoTag(${getterTerm.asExprOf[String]}) }
    else if t.isArrayByte then '{ ${os}.writeByteArrayNoTag(${getterTerm.asExprOf[Array[Byte]]}) }
    else if t.isArraySeqByte then '{ ${os}.writeByteArrayNoTag(${getterTerm.asExprOf[ArraySeq[Byte]]}.toArray[Byte]) }
    else if t.isBytesType then '{ ${os}.writeByteArrayNoTag(${getterTerm.asExprOf[IArray[Byte]]}.toArray) }
    else throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  private def sizeFun(t: TypeRepr & Matchable, getterTerm: Term): Expr[Int] =
    val CodedOutputStreamRef = Ref(TypeRepr.of[CodedOutputStream].typeSymbol.companionModule)
    if      t.isInt then '{ CodedOutputStream.computeInt32SizeNoTag(${getterTerm.asExprOf[Int]}) }
    else if t.isLong then '{ CodedOutputStream.computeInt64SizeNoTag(${getterTerm.asExprOf[Long]}) }
    else if t.isBoolean then Expr(1)
    else if t.isDouble then Expr(8)
    else if t.isFloat then Expr(4)
    else if t.isString then '{ CodedOutputStream.computeStringSizeNoTag(${getterTerm.asExprOf[String]}) }
    else if t.isArrayByte then '{ CodedOutputStream.computeByteArraySizeNoTag(${getterTerm.asExprOf[Array[Byte]]}) }
    else if t.isArraySeqByte then '{ CodedOutputStream.computeByteArraySizeNoTag(${getterTerm.asExprOf[ArraySeq[Byte]]}.toArray[Byte]) }
    else if t.isBytesType then '{ CodedOutputStream.computeByteArraySizeNoTag(${getterTerm.asExprOf[IArray[Byte]]}.toArray) }
    else throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  private def readFun(t: TypeRepr & Matchable, is: Expr[CodedInputStream]): Term =
    if      t.isInt then '{ ${is}.readInt32 }.asTerm
    else if t.isLong then '{ ${is}.readInt64 }.asTerm
    else if t.isBoolean then '{ ${is}.readBool }.asTerm
    else if t.isDouble then '{ ${is}.readDouble }.asTerm
    else if t.isFloat then '{ ${is}.readFloat }.asTerm
    else if t.isString then '{ ${is}.readString.nn }.asTerm
    else if t.isArrayByte then '{ ${is}.readByteArray.nn }.asTerm
    else if t.isArraySeqByte then '{ ArraySeq.unsafeWrapArray(${is}.readByteArray.nn) }.asTerm
    else if t.isBytesType then '{ IArray.unsafeFromArray(${is}.readByteArray.nn) }.asTerm
    else throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  private val ArrayByteType: TypeRepr = TypeRepr.of[Array[Byte]]
  private val ArraySeqByteType: TypeRepr = TypeRepr.of[ArraySeq[Byte]]
  private val BytesType: TypeRepr = TypeRepr.of[IArray[Byte]]
  private val NTpe: TypeRepr = TypeRepr.of[N]
  private val RestrictedNType: TypeRepr = TypeRepr.of[RestrictedN]
  private val ItetableType: TypeRepr = TypeRepr.of[scala.collection.Iterable[?]]
  private val PrepareType: TypeRepr = TypeRepr.of[Prepare]
  private val CodedInputStreamType: TypeRepr = TypeRepr.of[CodedInputStream]
  
  extension (s: Symbol)
    private def constructorParams: List[Symbol] = s.primaryConstructor.paramSymss.find(_.headOption.fold(false)( _.isTerm)).getOrElse(Nil)
    private def tpe: TypeRepr =
      s.tree match
        case x: ClassDef => x.constructor.returnTpt.tpe
        case ValDef(_,tpt,_) => tpt.tpe
        case Bind(_, pattern: Term) => pattern.tpe

  private def unitLiteral: Literal = Literal(UnitConstant())
  private def defaultMethodName(i: Int): String = s"$$lessinit$$greater$$default$$${i+1}"

  private def unitExpr: Expr[Unit] = unitLiteral.asExprOf[Unit]

  private def builderType: TypeRepr = TypeRepr.of[scala.collection.mutable.Builder]
    
  private def OptionType: TypeRepr = TypeRepr.of[Option]

  private def Some_Apply(tpe: TypeRepr, value: Term): Term =
    Select.unique(Ref(SomeModule.companionModule), "apply")
      .appliedToType(tpe)
      .appliedTo(value)

  private val commonTypes: List[TypeRepr] =
    TypeRepr.of[String] :: TypeRepr.of[Int] :: TypeRepr.of[Long] :: TypeRepr.of[Boolean] :: TypeRepr.of[Double] :: TypeRepr.of[Float] :: ArrayByteType :: ArraySeqByteType :: BytesType :: Nil 

  extension (t: TypeRepr & Matchable)
    private def isNType: Boolean = t =:= NTpe
    private def isCaseClass: Boolean = t.typeSymbol.flags.is(Flags.Case)
    private def isCaseObject: Boolean = t.termSymbol.flags.is(Flags.Case)
    private def isCaseType: Boolean = t.isCaseClass || t.isCaseObject
    private def isSealedTrait: Boolean = t.typeSymbol.flags.is(Flags.Sealed) && t.typeSymbol.flags.is(Flags.Trait)
    private def isIterable: Boolean = t <:< ItetableType && !t.isArraySeqByte
    private def isString: Boolean = t =:= TypeRepr.of[String]
    private def isInt: Boolean = t =:= TypeRepr.of[Int]
    private def isLong: Boolean = t =:= TypeRepr.of[Long]
    private def isBoolean: Boolean = t =:= TypeRepr.of[Boolean]
    private def isDouble: Boolean = t =:= TypeRepr.of[Double]
    private def isFloat: Boolean = t =:= TypeRepr.of[Float]
    private def isArrayByte: Boolean = t =:= ArrayByteType
    private def isArraySeqByte: Boolean = t =:= ArraySeqByteType
    private def isBytesType: Boolean = t =:= BytesType
    private def isCommonType: Boolean = commonTypes.exists(_ =:= t)

    private def typeArgsToReplace: Map[String, TypeRepr] =
      t.typeSymbol.primaryConstructor.paramSymss
      .find(_.headOption.fold(false)( _.isType))
      .map(_.map(_.name).zip(t.typeArgs)).getOrElse(Nil)
      .toMap

    private def replaceTypeArgs(map: Map[String, TypeRepr]): TypeRepr = t match
      case AppliedType(t1, args) =>
        t1.appliedTo(args.map(_.matchable.replaceTypeArgs(map)))
      case _ =>
        map.getOrElse(t.typeSymbol.name, t)

    private def isOption: Boolean = t match
      case AppliedType(t1, _) if t1.typeSymbol == OptionClass => true
      case _ => false

    private def typeArgs: List[TypeRepr] = t match
      case AppliedType(t1, args)  => args
      case _ => Nil

    private def optionArgument: TypeRepr = t match
      case AppliedType(t1, args) if t1.typeSymbol == OptionClass => args.head
      case _ => throwError(s"It isn't Option type: ${t.typeSymbol.name}")

    private def iterableArgument: TypeRepr = t.baseType(ItetableType.typeSymbol).matchable match
      case AppliedType(_, args) if t.isIterable => args.head
      case _ => throwError(s"It isn't Iterable type: ${t.typeSymbol.name}")

    private def iterableBaseType: TypeRepr = t match
      case AppliedType(t1, _) if t.isIterable => t1
      case _ => throwError(s"It isn't Iterable type: ${t.typeSymbol.name}")

    private def restrictedNums: List[Int] =
      val aName = RestrictedNType.typeSymbol.name
      val tName = t.typeSymbol.fullName
      t.typeSymbol.annotations.collect{ case Apply(Select(New(tpt),_), List(Typed(Repeated(args,_),_))) if tpt.tpe =:= RestrictedNType => args } match
        case List(Nil) => throwError(s"empty annotation ${aName} for `${tName}`")
        case List(xs) =>
          val nums = xs.collect{
            case Literal(IntConstant(n)) => n
            case x => throwError(s"wrong annotation ${aName} for `${tName}` $x")
          }
          if nums.size != nums.distinct.size then throwError(s"nums not unique in annotation ${aName} for `${tName}`")
          nums
        case Nil => Nil
        case _ => throwError(s"multiple ${aName} annotations applied for `${tName}`")

  private def mkIfStatement(branches: List[(Term, Term)], elseBranch: Term): Term =
    branches match
      case (cond, thenp) :: xs =>
        If(cond, thenp, mkIfStatement(xs, elseBranch))
      case Nil => elseBranch

end Impl
