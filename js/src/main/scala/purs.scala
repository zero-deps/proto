package zd
package proto

import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._

final case class Res(prelude: String, dataType: String, types: List[String], decoders: List[String]) {
  def format: String = prelude + "\n\n" + dataType + "\n\n" + types.mkString("\n") + "\n\n" + decoders.mkString("\n\n")
}

object Purescript {
  def generate[A](moduleName: String)(implicit ttag: TypeTag[A]): Res = {
    val datas = mutable.ListBuffer.empty[String]
    val types = mutable.ListBuffer.empty[String]
    val decoders = mutable.ListBuffer.empty[String]

    def findN(x: Symbol): String Either Int = {
      x.annotations.filter(_.tree.tpe == typeOf[zd.proto.api.N]) match {
        case List(x1) => x1.tree.children.tail match {
          case List(Literal(Constant(n: Int))) => Right(n)
          case _ => Left("bad args in N")
        }
        case Nil => Left(s"no N on ${x}")
        case _ => Left(s"multiple N on ${x}")
      }
    }

    def fields(tpe: Symbol): List[(String, Type, String Either Int)] = {
      tpe.asClass.primaryConstructor.asMethod.paramLists.flatten.map{ x =>
        val term = x.asTerm
        (term.name.decodedName.toString, term.info, findN(x))
      }
    }

    def isIterable(tpe: Type): Boolean = {
      tpe.baseClasses.exists(_.asType.toType.typeConstructor <:< typeOf[scala.collection.TraversableOnce[Unit]].typeConstructor)
    }
    
    def constructType(name: String, fieldsOf: List[(String, Type, String Either Int)]): String = {
      fieldsOf.map{ case (name, tpe, _) =>
        val pursType =
          if (tpe =:= StringClass.selfType) {
            "String"
          } else if (isIterable(tpe)) {
            val typeArg = tpe.typeArgs.head.typeSymbol
            val typeArgName = typeArg.asClass.name.decodedName.toString
            val typeArgFields = fields(typeArg.asType.toType.typeSymbol)
            types += constructType(typeArgName, typeArgFields)
            s"Array ${typeArgName}"
          } else {
            "?"+tpe.toString
          }
        s"${name} :: ${pursType}"
      }.mkString(s"type ${name} = { ", ", ", " }")
    }

    def constructDecode(name: String, fieldsOf: List[(String, Type, String Either Int)]): String = {
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
        case (name, tpe, Right(n)) =>
          if (tpe =:= StringClass.selfType) {
            List(
              s"${n} -> do"
            , s"  x <- string reader"
            , s"  decode end $$ acc { ${name} = x }"
            )
          } else if (isIterable(tpe)) {
            val typeArg = tpe.typeArgs.head.typeSymbol
            val typeArgName = typeArg.asClass.name.decodedName.toString
            val typeArgFields = fields(typeArg.asType.toType.typeSymbol)
            decoders += constructDecode(typeArgName, typeArgFields)
            List(
              s"${n} -> do"
            , s"  x <- uint32 reader >>= decode${typeArgName} reader"
            , s"  decode end $$ acc { ${name} = snoc acc.${name} x }"
            )
          } else {
            List("?"+tpe.toString)
          }
        case (_, _, Left(e)) =>
          throw new Exception(e)
      }.flatten
      s"""|decode${name} :: Reader -> Int -> Effect ${name}
          |decode${name} reader msglen = do
          |  let end = pos reader + msglen
          |  decode end ${defObj}
          |  where
          |    decode :: Int -> ${name} -> Effect ${name}
          |    decode end acc =
          |      if pos reader < end then do
          |        tag <- uint32 reader
          |        case zshr tag 3 of
          |${cases.map("          "+_).mkString("\n")}
          |          _ -> do
          |            skipType reader $$ tag .&. 7
          |            decode end acc
          |      else pure acc""".stripMargin
    }

    val traitTpe = typeOf[A]
    val clazz = traitTpe.typeSymbol.asClass
    val traitName = clazz.name.encodedName.toString
    def decodeTrait(name: String, clazz: ClassSymbol): String = {
      val cases = clazz.knownDirectSubclasses.map{ x =>
        findN(x) match {
          case Left(e) => throw new Exception(e)
          case Right(n) =>
            val subclassName = x.name.decodedName.toString
            List(
              s"${n} -> do"
            , s"  msglen <- uint32 reader"
            , s"  x <- decode${subclassName} reader msglen"
            , s"  pure $$ Just $$ ${subclassName} x"
            )
        }
      }.flatten
      s"""|decode${name} :: Uint8Array -> Effect (Maybe ${name})
          |decode${name} bytes = do
          |  let reader = createReader bytes
          |  tag <- uint32 reader
          |  case zshr tag 3 of
          |${cases.map("    "+_).mkString("\n")}
          |    _ ->
          |      pure Nothing""".stripMargin
    }
    decoders += decodeTrait(traitName, clazz)
    clazz.knownDirectSubclasses.map{ x =>
      val name = x.name.decodedName.toString
      datas += s"${name} ${name}"
      val tpe = x.asType.toType.typeSymbol
      types += constructType(name, fields(tpe))
      decoders += constructDecode(name, fields(tpe))
    }
    
    Res(
      prelude(moduleName),
      dataType = s"data ${traitName} = ${datas.mkString(" | ")}",
      types.toList,
      decoders.toList,
    )
  }

  def prelude(moduleName: String): String = s"""module ${moduleName} where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Prelude (bind, discard, pure, ($$), (+), (<), (>>=))
import Proto (Reader, createReader, pos, skipType, string, uint32)"""
}
