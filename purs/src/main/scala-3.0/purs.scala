package proto
package purs

import scala.quoted.*
import proto.*
import scala.collection.immutable.ArraySeq
import compiletime.asMatchable

inline def enumByN[A, B]: String = ${enumByN[A, B]}

def enumByN[A: Type, B: Type](using qctx: Quotes): Expr[String] = Impl().enumByN[A, B]

private class Impl(using qctx: Quotes):
  import qctx.reflect.{*, given}
  import qctx.reflect.defn.*
  import report.*

  def enumByN[A: Type, B: Type]: Expr[String] =
    val a_tpe = TypeRepr.of[A]
    val b_tpe = TypeRepr.of[B]
    val a_fields = fieldsOf(a_tpe.asMatchable)
    val b_fields = fieldsOf(b_tpe.asMatchable)
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
            if tpt.tpe.asMatchable.isNType =>
              x.tpe -> num
        } match
          case List(x) => x
          case Nil =>
            errorAndAbort(s"missing ${NTpe.typeSymbol.name} annotation for `$_typeName`")
          case _ =>
            errorAndAbort(s"multiple ${NTpe.typeSymbol.name} annotations applied for `$_typeName`")
      }

    if _subclasses.size <= 0
      then errorAndAbort(s"required at least 1 subclass for `${_typeName}`")
    if _nums.size != _subclasses.size
      then errorAndAbort(s"`${_typeName}` _subclasses ${_subclasses.size} count != _nums definition ${_nums.size}")
    if _nums.exists(_._2 < 1)
      then errorAndAbort(s"_nums for ${_typeName} should be > 0")
    if _nums.groupBy(_._2).exists(_._2.size != 1)
      then errorAndAbort(s"_nums for ${_typeName} should be unique")

    _subclasses.map{ s =>
      val tpe = s.tpe.asMatchable
      val num: Int = _nums.collectFirst{ case (tpe1, num) if tpe =:= tpe1 => num }.getOrElse(errorAndAbort(s"missing num for class `${tpe}` of trait `${_tpe}`"))
      if _tpe.restrictedNums.contains(num)
        then errorAndAbort(s"num ${num} is restricted for class `${tpe}` of trait `${_tpe}`")
    
      FieldInfo(
        name = s.name
      , num = num
      , sym = s
      , tpe = tpe
      , isCaseObject = s.isTerm
      )
    }

  private case class FieldInfo(
    name: String
  , num: Int
  , sym: Symbol
  , tpe: TypeRepr & Matchable
  , isCaseObject: Boolean = false
  )

  private val ArrayByteType: TypeRepr = TypeRepr.of[Array[Byte]]
  private val ArraySeqByteType: TypeRepr = TypeRepr.of[ArraySeq[Byte]]
  private val NTpe: TypeRepr = TypeRepr.of[N]
  private val RestrictedNType: TypeRepr = TypeRepr.of[RestrictedN]
  private val ItetableType: TypeRepr = TypeRepr.of[scala.collection.Iterable[?]]
  
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

  extension (t: TypeRepr)
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

    private def replaceTypeArgs(map: Map[String, TypeRepr]): TypeRepr = t.asMatchable match
      case AppliedType(t1, args) =>
        t1.appliedTo(args.map(_.asMatchable.replaceTypeArgs(map)))
      case _ =>
        map.getOrElse(t.typeSymbol.name, t)

    private def isOption: Boolean = t.asMatchable match
      case AppliedType(t1, _) if t1.typeSymbol == OptionClass => true
      case _ => false

    private def typeArgs: List[TypeRepr] = t.asMatchable match
      case AppliedType(t1, args)  => args
      case _ => Nil

    private def optionArgument: TypeRepr = t.asMatchable match
      case AppliedType(t1, args) if t1.typeSymbol == OptionClass => args.head
      case _ => errorAndAbort(s"It isn't Option type: ${t.typeSymbol.name}")

    private def iterableArgument: TypeRepr = t.baseType(ItetableType.typeSymbol).asMatchable match
      case AppliedType(_, args) if t.isIterable => args.head
      case _ => errorAndAbort(s"It isn't Iterable type: ${t.typeSymbol.name}")

    private def iterableBaseType: TypeRepr = t.asMatchable match
      case AppliedType(t1, _) if t.isIterable => t1
      case _ => errorAndAbort(s"It isn't Iterable type: ${t.typeSymbol.name}")

    private def restrictedNums: List[Int] =
      val aName = RestrictedNType.typeSymbol.name
      val tName = t.typeSymbol.fullName
      t.typeSymbol.annotations.collect{ case Apply(Select(New(tpt),_), List(Typed(Repeated(args,_),_))) if tpt.tpe =:= RestrictedNType => args } match
        case List(Nil) => errorAndAbort(s"empty annotation ${aName} for `${tName}`")
        case List(xs) =>
          val nums = xs.collect{
            case Literal(IntConstant(n)) => n
            case x => errorAndAbort(s"wrong annotation ${aName} for `${tName}` $x")
          }
          if nums.size != nums.distinct.size then errorAndAbort(s"nums not unique in annotation ${aName} for `${tName}`")
          nums
        case Nil => Nil
        case _ => errorAndAbort(s"multiple ${aName} annotations applied for `${tName}`")

  private def mkIfStatement(branches: List[(Term, Term)], elseBranch: Term): Term =
    branches match
      case (cond, thenp) :: xs =>
        If(cond, thenp, mkIfStatement(xs, elseBranch))
      case Nil => elseBranch

end Impl
