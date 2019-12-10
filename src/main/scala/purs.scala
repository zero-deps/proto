package zd.proto.purs

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.annotation.tailrec
import zd.proto.api.{MessageCodec}

final case class Res2(common: Res, encode: Res, decode: Res)
final case class Res(prelude: String, types: Seq[String], coders: Seq[String])
object Res {
  def format(res: Res): String = {
    res.prelude + "\n" +
    res.types.mkString("\n") + "\n\n" + res.coders.mkString("\n\n") + "\n"
  }
  def writeToFile(path: String, res: Res): Unit = {
    writeToFile(path: String, format(res))
  }
  def writeToFile(path: String, res: String): Unit = {
    import java.io.{BufferedWriter, FileWriter}
    val w = new BufferedWriter(new FileWriter(path))
    w.write(res)
    w.close()
  }
}

object Purescript {
  def generate[D, E](moduleEncodeName: String, moduleDecodeName: String, commonModule: String, codecs: List[MessageCodec[_]])(implicit dtag: TypeTag[D], etag: TypeTag[E]): Res2 = {
    val preludeCommon = s"""module ${commonModule} where

import Data.Map (Map)
import Data.Maybe (Maybe)
"""
    val preludeEncode = s"""module ${moduleEncodeName} where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, ($$))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll, fromArray)
import ${commonModule}
"""

    val preludeDecode = s"""module ${moduleDecodeName} where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right))
import Data.Int.Bits (zshr, (.&.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple), fst, snd)
import Prelude (bind, pure, ($$), (+), (<))
import Proto.Decode as Decode
import ${commonModule}
"""

    def findN(x: Symbol): Option[Int] = {
      x.annotations.filter(_.tree.tpe =:= typeOf[zd.proto.api.N])  match {
        case List(x1) => x1.tree.children.tail match {
          case List(Literal(Constant(n: Int))) => Some(n)
          case _ => throw new Exception("bad args in N")
        }
        case Nil => None
        case _ => throw new Exception(s"multiple N on ${x}")
      }
    }
    def fields(tpe: Type): List[(String, Type, Int)] = {
      tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.flatten.map{ x =>
        val term = x.asTerm
        (term.name.encodedName.toString, term.info, findN(x))
      }.collect{ case (a, b, Some(n)) => (a, b, n) }.sortBy(_._3)
    }
    def isTrait(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isTrait && t.typeSymbol.asClass.isSealed
    def findChildren(tpe: Type): Seq[(Type, Int)] = tpe.typeSymbol.asClass.knownDirectSubclasses.toVector.map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2).map{ case (x, n) => (x.asType.toType, n) }
    @tailrec def isRecursive(base: Type, compareTo: List[Type]): Boolean = compareTo match {
      case Nil => false
      case x :: _ if x =:= base => true
      case x :: xs => isRecursive(base, x.typeArgs.map(_.typeSymbol.asType.toType) ++ xs)
    }

    sealed trait Tpe { val tpe: Type }
    final case class TraitType(tpe: Type, children: Seq[(Type,Int)], firstLevel: Boolean) extends Tpe
    final case class RegularType(tpe: Type, recursive: Boolean) extends Tpe
    final case class TupleType(tpe: Type, tpe_1: Type, tpe_2: Type) extends Tpe // consider merging with RegularType

    def collectTypes(tpe: Type): Seq[Tpe] = {
      val complexType: Type => Boolean = {
        case tpe if tpe =:= StringClass.selfType => false
        case tpe if tpe =:= IntClass.selfType => false
        case tpe if tpe =:= BooleanClass.selfType => false
        case tpe if tpe =:= DoubleClass.selfType => false
        case tpe if tpe =:= typeOf[Array[Byte]] => false
        case _ => true
      }
      @tailrec def loop(head: Type, tail: Seq[Type], acc: Seq[Tpe], firstLevel: Boolean): Seq[Tpe] = {
        val (tail1, acc1): (Seq[Type], Seq[Tpe]) =
          if (acc.exists(_.tpe =:= head)) {
            (tail, acc)
          } else if (isTrait(head)) {
            val children = findChildren(head)
            (children.map(_._1)++tail, acc:+TraitType(head, children, firstLevel))
          } else if (head.typeConstructor =:= OptionClass.selfType.typeConstructor) {
            val typeArg = head.typeArgs.head.typeSymbol
            val typeArgType = typeArg.asType.toType
            if (complexType(typeArgType)) (typeArgType+:tail, acc)
            else (tail, acc)
          } else if (isIterable(head)) {
            head.typeArgs match {
              case x :: Nil =>
                val typeArg = x.typeSymbol
                val typeArgType = typeArg.asType.toType
                if (complexType(typeArgType)) (typeArgType+:tail, acc)
                else (tail, acc)
              case x :: y :: Nil =>
                (tail, acc:+TupleType(appliedType(typeOf[Tuple2[Unit, Unit]].typeConstructor, x, y), x, y))
              case _ => throw new Exception(s"too many type args for ${head}")
            }
          } else {
            val fs = fields(head).map(_._2).filter(complexType)
            (fs++tail, acc:+RegularType(head, isRecursive(head, fs)))
          }
        tail1 match {
          case h +: t => loop(h, t, acc1, false)
          case _ => acc1
        }
      }
      loop(tpe, Nil, Nil, true)
    }

    def makeEncodeTpe(x: Type): Seq[String] = makeTpe(x, genMaybe=false)
    def makeDecodeTpe(x: Type): Seq[String] = makeTpe(x, genMaybe=true)
    def makeTpe(x: Type, genMaybe: Boolean): Seq[String] = {
      collectTypes(x).flatMap{
        case TraitType(tpe, children,_) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          s"data ${name} = ${children.map(_._1.typeSymbol.name.encodedName.toString).map(x => s"${x} ${x}").mkString(" | ")}" +: Vector.empty
        case _: TupleType => Nil
        case RegularType(tpe, recursive) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val fieldsOf = fields(tpe)
          val fs = fieldsOf.map{ case (name1, tpe, _) => name1 -> pursType(tpe) }
          val typeDef = if (recursive) "newtype" else "type"
          def constructor(genMaybe: Boolean) = (genMaybe, recursive) match {
            case (true, true) => s"${name}' "
            case (false, true) => s"${name} "
            case (_, false) => ""
          }
          val x = fs.map{ case (name1, tpe) => s"${name1} :: ${tpe._1}" }.mkString(s"${typeDef} ${name} = ${constructor(false)}{ ", ", ", " }")
          val x1 = fs.map{ case (name1, tpe) => s"${name1} :: ${tpe._2}" }.mkString(s"${typeDef} ${name}' = ${constructor(true)}{ ", ", ", " }")
          if (genMaybe) Vector(x, x1) else Vector(x)
      }
    }

    def makeDecoders(tpe: Type): Seq[String] = {
      collectTypes(tpe).map{
        case TraitType(tpe, children, true) =>
          val cases = children.map{ case (tpe, n) =>
            val name = tpe.typeSymbol.name.encodedName.toString
            List(
              s"${n} -> do"
            , s"  { pos: pos2, val } <- decode${name} _xs_ pos1"
            , s"  pure { pos: pos2, val: ${name} val }"
            )
          }
          val name = tpe.typeSymbol.name.encodedName.toString
          s"""|decode${name} :: Uint8Array -> Decode.Result ${name}
              |decode${name} _xs_ = do
              |  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
              |  case tag `zshr` 3 of${cases.map(_.map("\n    "+_).mkString("")).mkString("")}
              |    i ->
              |      Left $$ Decode.BadType i""".stripMargin
        case TraitType(tpe, children, false) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val cases = children.flatMap{ case (tpe, n) =>
            val name = tpe.typeSymbol.name.encodedName.toString
            List(
              s"${n} ->"
            , s"  case decode${name} _xs_ pos2 of"
            , s"    Left x -> Left x"
            , s"    Right { pos: pos3, val } ->"
            , s"      decode end (Just $$ ${name} val) pos3"
            )
          }
          s"""|decode${name} :: Uint8Array -> Int -> Decode.Result ${name}
              |decode${name} _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  decode end Nothing pos
              |    where
              |    decode :: Int -> Maybe ${name} -> Int -> Decode.Result ${name}
              |    decode end acc pos1 | pos1 < end =
              |      case Decode.uint32 _xs_ pos1 of
              |        Left x -> Left x
              |        Right { pos: pos2, val: tag } ->
              |          case tag `zshr` 3 of${cases.map("\n            "+_).mkString}
              |            _ ->
              |              case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
              |                Left x -> Left x
              |                Right { pos: pos3 } ->
              |                  decode end acc pos3
              |    decode end (Just acc) pos1 = pure { pos: pos1, val: acc }
              |    decode end acc@Nothing pos1 = Left $$ Decode.MissingFields "${name}"""".stripMargin
        case TupleType(tpe, tpe_1, tpe_2) =>
          val tt = tpe.toString
          val xs = codecs.find(_.aType == tt).map(_.nums).getOrElse(throw new Exception(s"codec is missing for ${tt}"))
          val fun = "decode" + tupleFunName(tpe_1, tpe_2)
          val cases = List(("first", tpe_1, xs("_1")), ("second", tpe_2, xs("_2"))).flatMap((decodeField(None) _).tupled)
          s"""$fun :: Uint8Array -> Int -> Decode.Result (Tuple ${pursTypePars(tpe_1)._1} ${pursTypePars(tpe_2)._1})
$fun _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { ${nothingValue("first", tpe_1)}, ${nothingValue("second", tpe_2)} } pos
  case val of
    { ${justValue("first", tpe_1)}, ${justValue("second", tpe_2)} } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $$ Decode.MissingFields "$fun"
    where
    decode :: Int -> { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} } -> Int -> Decode.Result { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} }
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of${cases.map("\n              "+_).mkString("")}
              _ ->
                case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""
        case RegularType(tpe, recursive) =>
          val fs = fields(tpe)
          val defObj: String = fs.map{ case (name, tpe, _) => nothingValue(name, tpe) }.mkString("{ ", ", ", " }")
          val justObj: String = fs.map{ case (name, tpe, _) => justValue(name, tpe) }.mkString("{ ", ", ", " }")
          val unObj: String = fs.map{
            case (name,_,_) => name
          }.mkString("{ ", ", ", " }")
          val name = tpe.typeSymbol.name.encodedName.toString
          val cases = fields(tpe).flatMap((decodeField(Some(name).filter(_ => recursive)) _).tupled)
          s"""|decode${name} :: Uint8Array -> Int -> Decode.Result ${name}
              |decode${name} _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  { pos: pos1, ${if (recursive) s"val: ${name}' val" else "val"} } <- decode end ${if (recursive) s"(${name}' ${defObj})" else s"${defObj}"} pos
              |  case val of
              |    ${justObj} -> pure { pos: pos1, val: ${if (recursive) s"${name} " else ""}${unObj} }${if (justObj==unObj) "" else s"""\n    _ -> Left $$ Decode.MissingFields "${name}""""}
              |    where
              |    decode :: Int -> ${name}' -> Int -> Decode.Result ${name}'
              |    decode end ${if (recursive) s"(${name}' acc)" else "acc"} pos1 =
              |      if pos1 < end then
              |        case Decode.uint32 _xs_ pos1 of
              |          Left x -> Left x
              |          Right { pos: pos2, val: tag } ->
              |            case tag `zshr` 3 of${cases.map("\n              "+_).mkString("")}
              |              _ ->
              |                case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
              |                  Left x -> Left x
              |                  Right { pos: pos3 } ->
              |                    decode end ${if (recursive) s"(${name}' acc)" else "acc"} pos3
              |      else pure { pos: pos1, val: ${if (recursive) s"${name}' acc" else "acc"} }""".stripMargin
      }
    }

    val decodeTypes = makeDecodeTpe(typeOf[D]).distinct
    val decoders = makeDecoders(typeOf[D]).distinct

    def makeEncoders(tpe: Type): Seq[String] = {
      collectTypes(tpe).map{
        case TraitType(tpe, children,true) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val cases = children.map{ case (tpe, n) =>
            val name1 = tpe.typeSymbol.name.encodedName.toString
            List(
              s"encode${name} (${name1} x) = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode${name1} x ]"
            )
          }
          s"""|encode${name} :: ${name} -> Uint8Array
              |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
        case TraitType(tpe, children,false) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val cases = children.map{ case (tpe, n) =>
            val name1 = tpe.typeSymbol.name.encodedName.toString
            List(
              List(
                s"encode${name} (${name1} x) = do"
              , s"  let xs = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode${name1} x ]"
              , s"  let len = length xs"
              , s"  concatAll [ Encode.uint32 len, xs ]"
              ).mkString("\n")
            )
          }
          s"""|encode${name} :: ${name} -> Uint8Array
              |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
        case TupleType(tpe, tpe_1, tpe_2) =>
          val tt = tpe.toString
          val xs = codecs.find(_.aType == tt).map(_.nums).getOrElse(throw new Exception(s"codec is missing for ${tt}"))
          val fun = "encode" + tt.filter(_.isLetter)
          s"""$fun :: Tuple ${pursTypePars(tpe_1)._1} ${pursTypePars(tpe_2)._1} -> Uint8Array
$fun (Tuple _1 _2) = do
  let msg = { _1, _2 }
  let xs = concatAll
        ${List(("_1", tpe_1, xs("_1")), ("_2", tpe_2, xs("_2"))).flatMap((encodeField _).tupled).mkString("[ ", "\n        , ", "\n        ]")}
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]"""
        case RegularType(tpe, recursive) =>
          val encodeFields = fields(tpe).flatMap((encodeField _).tupled)
          val name = tpe.typeSymbol.name.encodedName.toString
          if (encodeFields.nonEmpty) {
            s"""|encode${name} :: ${name} -> Uint8Array
                |encode${name} ${if (recursive) s"(${name} msg)" else "msg"} = do
                |  let xs = concatAll
                |  ${encodeFields.mkString("      [ ", "\n        , ", "\n        ]")}
                |  let len = length xs
                |  concatAll [ Encode.uint32 len, xs ]""".stripMargin
          } else {
            s"""|encode${name} :: ${name} -> Uint8Array
                |encode${name} _ = Encode.uint32 0""".stripMargin
          }
      }
    }

    val encodeTypes = makeEncodeTpe(typeOf[E]).distinct
    val encoders = makeEncoders(typeOf[E]).distinct

    val commonTypes = encodeTypes.intersect(decodeTypes)
    Res2(
      common=Res(preludeCommon, commonTypes, Nil)
    , encode=Res(
      preludeEncode,
      encodeTypes.diff(commonTypes),
      encoders,
    ), decode=Res(
      preludeDecode,
      decodeTypes.diff(commonTypes),
      decoders,
    ))
  }

}
