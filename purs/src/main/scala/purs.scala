package zd
package proto

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.annotation.tailrec

final case class Res2(common: Res, encode: Res, decode: Res)
final case class Res(prelude: String, types: Seq[String], coders: Seq[String])
object Res {
  def format(res: Res): String = {
    res.prelude + "\n" +
    res.types.mkString("\n") + "\n\n" + res.coders.mkString("\n\n") + "\n"
  }
  def writeToFile(path: String, res: Res): Unit = {
    import java.io.{BufferedWriter, FileWriter}
    val w = new BufferedWriter(new FileWriter(path))
    w.write(format(res))
    w.close()
  }
}

object Purescript {
  def generate[D, E](moduleEncodeName: String, moduleDecodeName: String, commonModule: String)(implicit dtag: TypeTag[D], etag: TypeTag[E]): Res2 = {
    val preludeCommon = s"""module ${commonModule} where

import Data.Map (Map)
import Data.Maybe (Maybe)
"""
    val preludeEncode = s"""module ${moduleEncodeName} where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, ($$))
import Proto.Encode as Encode
import Uint8ArrayExt (length, concatAll, fromArray)
import ${commonModule}

encodeStringString :: Tuple String String -> Uint8Array
encodeStringString (Tuple k v) = concatAll [ Encode.uint32 10, Encode.string k, Encode.uint32 18, Encode.string v ]
"""

    val preludeDecode = s"""module ${moduleDecodeName} where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int.Bits (zshr, (.&.))
import Prelude (bind, pure, ($$), (+), (<))
import Proto.Decode as Decode
import ${commonModule}

decodeStringString :: Uint8Array -> Int -> Decode.Result { first :: String, second :: String }
decodeStringString _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { first: Nothing, second: Nothing } pos
  case val of
    { first: Just first, second: Just second } -> pure { pos: pos1, val: { first, second } }
    _ -> Left $$ Decode.MissingFields "StringString"
    where
    decode :: Int -> { first :: Maybe String, second :: Maybe String } -> Int -> Decode.Result { first :: Maybe String, second :: Maybe String }
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { first = Just val }) pos3
              2 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { second = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }
"""

    def findN(x: Symbol): Option[Int] = {
      x.annotations.filter(_.tree.tpe == typeOf[zd.proto.api.N]) match {
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
    def isIterable(tpe: Type): Boolean = {
      tpe.baseClasses.exists(_.asType.toType.typeConstructor <:< typeOf[scala.collection.TraversableOnce[Unit]].typeConstructor)
    }
    def isTrait(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isTrait && t.typeSymbol.asClass.isSealed
    def findChildren(tpe: Type): Seq[(Type, Int)] = tpe.typeSymbol.asClass.knownDirectSubclasses.toVector.map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2).map{ case (x, n) => (x.asType.toType, n) }

    sealed trait Tpe
    final case class TraitType(tpe: Type, children: Seq[(Type,Int)], firstLevel: Boolean) extends Tpe
    final case class SimpleType(tpe: Type) extends Tpe

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
        val (tail1, acc1) =
          if (isTrait(head)) {
            val children = findChildren(head)
            (children.map(_._1)++tail, acc:+TraitType(head, children, firstLevel))
          } else if (head.typeConstructor =:= OptionClass.selfType.typeConstructor) {
            val typeArg = head.typeArgs.head.typeSymbol
            val typeArgType = typeArg.asType.toType
            if (complexType(typeArgType)) (typeArgType+:tail, acc)
            else (tail, acc)
          } else if (head =:= typeOf[Map[String,String]]) {
            (tail, acc)
          } else if (isIterable(head)) {
            val typeArg = head.typeArgs.head.typeSymbol
            val typeArgType = typeArg.asType.toType
            if (complexType(typeArgType)) (typeArgType+:tail, acc)
            else (tail, acc)
          } else {
            val fs = fields(head).map(_._2).filter(complexType)
            (fs++tail, acc:+SimpleType(head))
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
        case SimpleType(tpe) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val fieldsOf = fields(tpe)
          val fs = fieldsOf.map{ case (name1, tpe, _) =>
          val pursType =
            if (tpe =:= StringClass.selfType) {
              "String" -> "Maybe String"
            } else if (tpe =:= IntClass.selfType) {
              "Int" -> "Maybe Int"
            } else if (tpe =:= BooleanClass.selfType) {
              "Boolean" -> "Maybe Boolean"
            } else if (tpe =:= DoubleClass.selfType) {
              "Number" -> "Maybe Number"
            } else if (tpe =:= typeOf[Array[Byte]]) {
              "Uint8Array" -> "Maybe Uint8Array"
            } else if (tpe =:= typeOf[Map[String,String]]) {
              "Map String String" -> "Map String String"
            } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
              val typeArg = tpe.typeArgs.head
              if (typeArg =:= DoubleClass.selfType) {
                "Maybe Number" -> "Maybe Number"
              } else {
                val name = typeArg.typeSymbol.name.encodedName.toString
                s"Maybe ${name}" -> s"Maybe ${name}"
              }
            } else if (isIterable(tpe)) {
              val typeArg = tpe.typeArgs.head.typeSymbol
              val name = typeArg.asClass.name.encodedName.toString
              s"Array ${name}" -> s"Array ${name}"
            } else {
              val name = tpe.typeSymbol.name.encodedName.toString
              name -> s"Maybe ${name}"
            }
          name1 -> pursType
        }
        val x = fs.map{ case (name1, tpe) => s"${name1} :: ${tpe._1}" }.mkString(s"type ${name} = { ", ", ", " }")
        val x1 = fs.map{ case (name1, tpe) => s"${name1} :: ${tpe._2}" }.mkString(s"type ${name}' = { ", ", ", " }")
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
        case SimpleType(tpe) =>
          val fs = fields(tpe)
          val defObj: String = fs.map{
            case (name, tpe, _) =>
              if (tpe =:= typeOf[Map[String,String]]) {
                s"${name}: Map.empty"
              } else if (isIterable(tpe)) {
                s"${name}: []"
              } else {
                s"${name}: Nothing"
              }
          }.mkString("{ ", ", ", " }")
          val justObj: String = fs.map{
            case (name, tpe, _) =>
              if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
                name
              } else if (isIterable(tpe)) {
                name
              } else {
                s"${name}: Just ${name}"
              }
          }.mkString("{ ", ", ", " }")
          val unObj: String = fs.map{
            case (name,_,_) => name
          }.mkString("{ ", ", ", " }")
          def decodeTmpl(n: Int, fun: String, mod: String): Seq[String] = {
            List(
              s"${n} ->"
            , s"  case ${fun} _xs_ pos2 of"
            , s"    Left x -> Left x"
            , s"    Right { pos: pos3, val } ->"
            , s"      decode end (acc { ${mod} }) pos3"
            )
          }
          val cases = fields(tpe).map{ case (name, tpe, n) =>
            if (tpe =:= StringClass.selfType) {
              decodeTmpl(n, "Decode.string", s"${name} = Just val")
            } else if (tpe =:= IntClass.selfType) {
              decodeTmpl(n, "Decode.int32", s"${name} = Just val")
            } else if (tpe =:= BooleanClass.selfType) {
              decodeTmpl(n, "Decode.boolean", s"${name} = Just val")
            } else if (tpe =:= DoubleClass.selfType) {
              decodeTmpl(n, "Decode.double", s"${name} = Just val")
            } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
              val typeArg = tpe.typeArgs.head.typeSymbol
              val tpe1 = typeArg.asType.toType
              if (tpe1 =:= StringClass.selfType) {
                decodeTmpl(n, "Decode.string", s"${name} = Just val")
              } else if (tpe1 =:= IntClass.selfType) {
                decodeTmpl(n, "Decode.int32", s"${name} = Just val")
              } else if (tpe1 =:= BooleanClass.selfType) {
                decodeTmpl(n, "Decode.boolean", s"${name} = Just val")
              } else if (tpe1 =:= DoubleClass.selfType) {
                decodeTmpl(n, "Decode.double", s"${name} = Just val")
              } else {
                val typeArgName = typeArg.name.encodedName.toString
                decodeTmpl(n, s"decode${typeArgName}", s"${name} = Just val")
              }
            } else if (tpe =:= typeOf[Map[String,String]]) {
              decodeTmpl(n, "decodeStringString", s"${name} = Map.insert val.first val.second acc.${name}")
            } else if (isIterable(tpe)) {
              val typeArg = tpe.typeArgs.head.typeSymbol
              val typeArgType = typeArg.asType.toType
              if (typeArgType =:= StringClass.selfType) {
                decodeTmpl(n, "Decode.string", s"${name} = snoc acc.${name} val")
              } else {
                val typeArgName = typeArg.asClass.name.encodedName.toString
                decodeTmpl(n, s"decode${typeArgName}", s"${name} = snoc acc.${name} val")
              }
            } else {
              val name1 = tpe.typeSymbol.name.encodedName.toString
              decodeTmpl(n, s"decode${name1}", s"${name} = Just val")
            }
          }.flatten
          val name = tpe.typeSymbol.name.encodedName.toString
          s"""|decode${name} :: Uint8Array -> Int -> Decode.Result ${name}
              |decode${name} _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  { pos: pos1, val } <- decode end ${defObj} pos
              |  case val of
              |    ${justObj} -> pure { pos: pos1, val: ${unObj} }${if (justObj==unObj) "" else s"""\n    _ -> Left $$ Decode.MissingFields "${name}""""}
              |    where
              |    decode :: Int -> ${name}' -> Int -> Decode.Result ${name}'
              |    decode end acc pos1 =
              |      if pos1 < end then
              |        case Decode.uint32 _xs_ pos1 of
              |          Left x -> Left x
              |          Right { pos: pos2, val: tag } ->
              |            case tag `zshr` 3 of${cases.map("\n              "+_).mkString("")}
              |              _ ->
              |                case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
              |                  Left x -> Left x
              |                  Right { pos: pos3 } ->
              |                    decode end acc pos3
              |      else pure { pos: pos1, val: acc }""".stripMargin
      }
    }

    val decodeTypes = makeDecodeTpe(typeOf[D]).distinct
    val decoders = makeDecoders(typeOf[D]).distinct

    def makeEncoders(tpe: Type): Seq[String] = {
      collectTypes(tpe).map{
        case TraitType(tpe, children,_) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val cases = children.map{ case (tpe, n) =>
            val name1 = tpe.typeSymbol.name.encodedName.toString
            List(
              s"encode${name} (${name1} x) = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode${name1} x ]"
            )
          }
          s"""|encode${name} :: ${name} -> Uint8Array
              |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
        case SimpleType(tpe) =>
          val encodeFields = fields(tpe).flatMap{ case (name, tpe, n) =>
            if (tpe =:= StringClass.selfType) {
              List(
                s"""Encode.uint32 ${(n<<3)+2}"""
              , s"""Encode.string msg.${name}"""
              )
            } else if (tpe =:= IntClass.selfType) {
              List(
                s"""Encode.uint32 ${(n<<3)+0}"""
              , s"""Encode.uint32 msg.${name}"""
              )
            } else if (tpe =:= BooleanClass.selfType) {
              List(
                s"""Encode.uint32 ${(n<<3)+0}"""
              , s"""Encode.boolean msg.${name}"""
              )
            } else if (tpe =:= DoubleClass.selfType) {
              List(
                s"""Encode.uint32 ${(n<<3)+1}"""
              , s"""Encode.double msg.${name}"""
              )
            } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
              val typeArg = tpe.typeArgs.head.typeSymbol
              val tpe1 = typeArg.asType.toType
              if (tpe1 =:= StringClass.selfType) {
                s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+2}, Encode.string x ]) msg.${name}""" :: Nil
              } else if (tpe1 =:= IntClass.selfType) {
                s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+0}, Encode.uint32 x ]) msg.${name}""" :: Nil
              } else if (tpe1 =:= BooleanClass.selfType) {
                s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+0}, Encode.boolean x ]) msg.${name}""" :: Nil
              } else if (tpe1 =:= DoubleClass.selfType) {
                s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+1}, Encode.double x ]) msg.${name}""" :: Nil
              } else {
                val typeArgName = typeArg.name.encodedName.toString
                s"""fromMaybe (fromArray []) $$ map encode${typeArgName} msg.${name}""" :: Nil
              }
            } else if (tpe =:= typeOf[Array[Byte]]) {
              List(
                s"""Encode.uint32 ${(n<<3)+2}"""
              , s"""Encode.bytes msg.${name}"""
              )
            } else if (tpe =:= typeOf[Map[String,String]]) {
              s"concatAll $$ map encodeStringString $$ Map.toUnfoldableUnordered msg.${name}" :: Nil
            } else if (isIterable(tpe)) {
              val typeArg = tpe.typeArgs.head.typeSymbol
              val typeArgName = typeArg.asClass.name.encodedName.toString
              val typeArgType = typeArg.asType.toType
              if (typeArgType =:= StringClass.selfType) {
                List(
                  s"""concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, Encode.string x ]) msg.${name}""",
                )
              } else {
                s"""concatAll $$ map encode${typeArgName} msg.${name}""" :: Nil
              }
            } else {
              val tpeName = tpe.typeSymbol.name.encodedName.toString
              s"encode${tpeName} msg.${name}" :: Nil
            }
          }
          val name = tpe.typeSymbol.name.encodedName.toString
          if (encodeFields.nonEmpty) {
            s"""|encode${name} :: ${name} -> Uint8Array
                |encode${name} msg = do
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
