package purs

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted.*
import scala.collection.immutable.ArraySeq

inline def enumByN[A, B]: String = ${Macro.enumByN[A, B]}

object Macro:
  def enumByN[A: Type, B: Type](using qctx: Quotes): Expr[String] = Impl().enumByN[A, B]

private class Impl(using val qctx: Quotes) extends BuildCodec:
  import qctx.reflect.{*, given}
  import qctx.reflect.defn.*
  import report.*

  def enumByN[A: Type, B: Type]: Expr[String] =
    val a_tpe = TypeRepr.of[A]
    val b_tpe = TypeRepr.of[A]
    sealedTraitCodec(a_tpe, b_tpe)

  private def collectNums(_tpe: TypeRepr): Seq[(TypeRepr, Int)] =
    val _typeSymbol = _tpe.typeSymbol
    val _typeName = _typeSymbol.fullName
    _typeSymbol.children.map{ x =>
      x.annotations.collect{
        case Apply(Select(New(tpt),_), List(Literal(IntConstant(num)))) if tpt.tpe.isNType =>
          x.tpe -> num
      } match
        case List(x) => x
        case Nil =>
          throwError(s"missing ${NTpe.typeSymbol.name} annotation for `$_typeName`")
        case _ =>
          throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `$_typeName`")
    }

  private def collectFields(
    _tpe: TypeRepr
  ): List[FieldInfo] =
    val _nums = collectNums(_tpe)
    val _typeSymbol = _tpe.typeSymbol
    val _typeName = _typeSymbol.fullName
    val _subclasses = _typeSymbol.children

    if _subclasses.size <= 0
      then throwError(s"required at least 1 subclass for `${_typeName}`")
    if _nums.size != _subclasses.size
      then throwError(s"`${_typeName}` _subclasses ${_subclasses.size} count != _nums definition ${_nums.size}")
    if _nums.exists(_._2 < 1)
      then throwError(s"_nums for ${_typeName} should be > 0")
    if _nums.groupBy(_._2).exists(_._2.size != 1)
      then throwError(s"_nums for ${_typeName} should be unique")

    _subclasses.map{ s =>
      val tpe = s.tpe
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

  def sealedTraitCodec(
    a_tpe: TypeRepr
  , b_tpe: TypeRepr
  ): Expr[String] =
    val a_fields = collectFields(a_tpe)
    val b_fields = collectFields(b_tpe)
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

end Impl
