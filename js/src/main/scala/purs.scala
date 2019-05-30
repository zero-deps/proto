package zd
package proto

import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._

final case class Res(prelude: String, decodeData: String, encodeData: String, decodeTypes: List[String], encodeTypes: List[String], decoders: List[String], encoders: List[String]) {
  def format: String = prelude + "\n\n" + 
    decodeData + "\n" + decodeTypes.mkString("\n") + "\n\n" + decoders.mkString("\n\n") + "\n\n" +
    encodeData + "\n" + encodeTypes.mkString("\n") + "\n\n" + encoders.mkString("\n\n") +
    "\n"
}

object Purescript {
  def generate[D, E](moduleName: String)(implicit dtag: TypeTag[D], etag: TypeTag[E]): Res = {
    val decodeData = mutable.ListBuffer.empty[String]
    val encodeData = mutable.ListBuffer.empty[String]
    val decodeTypes = mutable.ListBuffer.empty[String]
    val encodeTypes = mutable.ListBuffer.empty[String]
    val decoders = mutable.ListBuffer.empty[String]
    val encoders = mutable.ListBuffer.empty[String]

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

    def fields(tpe: Symbol): List[(String, Type, Int)] = {
      tpe.asClass.primaryConstructor.asMethod.paramLists.flatten.map{ x =>
        val term = x.asTerm
        (term.name.encodedName.toString, term.info, findN(x))
      }.collect{ case (a, b, Some(n)) => (a, b, n) }.sortBy(_._3)
    }

    def isIterable(tpe: Type): Boolean = {
      tpe.baseClasses.exists(_.asType.toType.typeConstructor <:< typeOf[scala.collection.TraversableOnce[Unit]].typeConstructor)
    }

    def constructType(name: String, fieldsOf: List[(String, Type, Any)]): String = {
      fieldsOf.map{ case (name, tpe, _) =>
        val pursType =
          if (tpe =:= StringClass.selfType) {
            "String"
          } else if (tpe =:= typeOf[Array[Byte]]) {
            "Uint8Array"
          } else if (isIterable(tpe)) {
            val typeArg = tpe.typeArgs.head.typeSymbol
            val typeArgName = typeArg.asClass.name.encodedName.toString
            val typeArgType = typeArg.asType.toType
            val typeArgFields = fields(typeArgType.typeSymbol)
            if (typeArgType =:= StringClass.selfType) ()
            else decodeTypes += constructType(typeArgName, typeArgFields)
            s"Array ${typeArgName}"
          } else {
            "?"+tpe.toString
          }
        s"${name} :: ${pursType}"
      }.mkString(s"type ${name} = { ", ", ", " }")
    }

    def constructEncode(name: String, fieldsOf: List[(String, Type, Int)]): String = {
      val encodeFields = fieldsOf.flatMap{ case (name, tpe, n) => 
        if (tpe =:= StringClass.selfType) {
          List(
            s"""Encode.uint32 ${(n<<3)+2}"""
          , s"""Encode.string msg.${name}"""
          )
        } else if (tpe =:= typeOf[Array[Byte]]) {
          List(
            s"""Encode.uint32 ${(n<<3)+2}"""
          , s"""Encode.bytes msg.${name}"""
          )
        } else if (isIterable(tpe)) {
          val typeArg = tpe.typeArgs.head.typeSymbol
          val typeArgName = typeArg.asClass.name.encodedName.toString
          val typeArgType = typeArg.asType.toType
          if (typeArgType =:= StringClass.selfType) {
            List(
              s"""concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, Encode.string x ]) msg.${name}""",
            )
          } else {
            s"?unknown_typearg_${typeArgName}_for_encoder_${name}" :: Nil
          }
        } else {
          "?"+tpe.toString :: Nil
        }
      }
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

    def constructDecode(name: String, fieldsOf: List[(String, Type, Int)]): String = {
      val defObj = fieldsOf.map{
        case (name, tpe, _) =>
          if (tpe =:= StringClass.selfType) {
            s"""${name}: """""
          } else if (isIterable(tpe)) {
            s"${name}: []"
          } else {
            "?"+tpe.toString
          }
      }.mkString("{ ", ", ", " }")
      val cases = fieldsOf.map{
        case (name, tpe, n) =>
          if (tpe =:= StringClass.selfType) {
            List(
              s"${n} -> do"
            , s"  { pos: pos3, val } <- Decode.string xs pos2"
            , s"  decode end (acc { ${name} = val }) pos3"
            )
          } else if (isIterable(tpe)) {
            val typeArg = tpe.typeArgs.head.typeSymbol
            val typeArgName = typeArg.asClass.name.encodedName.toString
            val typeArgType = typeArg.asType.toType
            if (typeArgType =:= StringClass.selfType) {
              List(
                s"${n} -> do"
              , s"  { pos: pos3, val } <- Decode.string xs pos2"
              , s"  decode end (acc { ${name} = snoc acc.${name} val }) pos3"
              )
            } else {
              val typeArgFields = fields(typeArg.asType.toType.typeSymbol)
              decoders += constructDecode(typeArgName, typeArgFields)
              List(
                s"${n} -> do"
              , s"  { pos: pos3, val: msglen1 } <- Decode.uint32 xs pos2"
              , s"  { pos: pos4, val } <- decode${typeArgName} xs pos3 msglen1"
              , s"  decode end (acc { ${name} = snoc acc.${name} val }) pos4"
              )
            }
          } else {
            List("?"+tpe.toString)
          }
      }.flatten
      s"""|decode${name} :: Uint8Array -> Int -> Int -> Decode.Result ${name}
          |decode${name} xs pos msglen = do
          |  let end = pos + msglen
          |  decode end ${defObj} pos
          |    where
          |    decode :: Int -> ${name} -> Int -> Decode.Result ${name}
          |    decode end acc pos1 =
          |      if pos1 < end then do
          |        { pos: pos2, val: tag } <- Decode.uint32 xs pos1
          |        case tag `zshr` 3 of
          |${cases.map("          "+_).mkString("\n")}
          |          _ -> do
          |            { pos: pos3 } <- Decode.skipType xs pos2 $$ tag .&. 7
          |            decode end acc pos3
          |      else pure { pos: pos1, val: acc }""".stripMargin
    }

    val decodeTpe = typeOf[D]
    val decodeClass = decodeTpe.typeSymbol.asClass
    val decodeName = decodeClass.name.encodedName.toString
    val decodeSubclasses = decodeClass.knownDirectSubclasses.toList.map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2)
    decoders += {
      val cases = decodeSubclasses.map{ case (x, n) =>
        val subclassName = x.name.encodedName.toString
        List(
          s"${n} -> do"
        , s"  { pos: pos2, val: msglen } <- Decode.uint32 xs pos1"
        , s"  { pos: pos3, val } <- decode${subclassName} xs pos2 msglen"
        , s"  pure { pos: pos3, val: ${subclassName} val }"
        )
      }
      s"""|decode${decodeName} :: Uint8Array -> Decode.Result ${decodeName}
          |decode${decodeName} xs = do
          |  { pos: pos1, val: tag } <- Decode.uint32 xs 0
          |  case tag `zshr` 3 of
          |${cases.map(_.map("    "+_).mkString("\n")).mkString("\n")}
          |    i ->
          |      Left $$ Decode.BadType i""".stripMargin
    }
    decodeSubclasses.map{ case (x, _) =>
      val name = x.name.encodedName.toString
      decodeData += s"${name} ${name}"
      val tpe = x.asType.toType.typeSymbol
      decodeTypes += constructType(name, fields(tpe))
      decoders += constructDecode(name, fields(tpe))
    }

    val encodeTpe = typeOf[E]
    val encodeClass = encodeTpe.typeSymbol.asClass
    val encodeName = encodeClass.name.encodedName.toString
    val encodeSubclasses = encodeClass.knownDirectSubclasses.toList.map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2)
    encoders += {
      val cases = encodeSubclasses.map{ case (x, n) =>
        val subclassName = x.name.encodedName.toString
        List(
          s"encode${encodeName} (${subclassName} x) = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode${subclassName} x ]"
        )
      }
      s"""|encode${encodeName} :: ${encodeName} -> Uint8Array
          |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
    }
    encodeSubclasses.map{ case (x, _) =>
      val name = x.name.encodedName.toString
      encodeData += s"${name} ${name}"
      val tpe = x.asType.toType.typeSymbol
      encodeTypes += constructType(name, fields(tpe))
      encoders += constructEncode(name, fields(tpe))
    }

    Res(
      prelude(moduleName),
      decodeData = s"data ${decodeName} = ${decodeData.mkString(" | ")}",
      encodeData = s"data ${encodeName} = ${encodeData.mkString(" | ")}",
      decodeTypes.toList,
      encodeTypes.toList,
      decoders.toList,
      encoders.toList,
    )
  }

  def prelude(moduleName: String): String = s"""module ${moduleName} where

import Data.Array (snoc, concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left))
import Data.Int.Bits (zshr, (.&.))
import Prelude (bind, pure, ($$), (+), (<))
import Proto.Encode as Encode
import Proto.Decode as Decode
import Uint8ArrayExt (length, concatAll)"""
}
