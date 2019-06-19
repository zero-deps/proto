package zd
package proto

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.annotation.tailrec

final case class Res(prelude: String, decodeTypes: Seq[String], encodeTypes: Seq[String], decoders: Seq[String], encoders: Seq[String])
object Res {
  def format(res: Res): String = {
    res.prelude + "\n" +
    res.decodeTypes.mkString("\n") + "\n\n" + res.decoders.mkString("\n\n") + "\n\n" +
    res.encodeTypes.mkString("\n") + "\n\n" + res.encoders.mkString("\n\n") + "\n"
  }
  def writeToFile(path: String, res: Res): Unit = {
    import java.io.{BufferedWriter, FileWriter}
    val w = new BufferedWriter(new FileWriter(path))
    w.write(format(res))
    w.close()
  }
}

object Purescript {
  def generate[D, E](moduleName: String)(implicit dtag: TypeTag[D], etag: TypeTag[E]): Res = {
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
    def fields1(tpe: Type): List[(String, Type, Int)] = fields(tpe.typeSymbol)
    def fields(tpe: Symbol): List[(String, Type, Int)] = {
      tpe.asClass.primaryConstructor.asMethod.paramLists.flatten.map{ x =>
        val term = x.asTerm
        (term.name.encodedName.toString, term.info, findN(x))
      }.collect{ case (a, b, Some(n)) => (a, b, n) }.sortBy(_._3)
    }
    def isIterable(tpe: Type): Boolean = {
      tpe.baseClasses.exists(_.asType.toType.typeConstructor <:< typeOf[scala.collection.TraversableOnce[Unit]].typeConstructor)
    }
    def isTrait(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isTrait && t.typeSymbol.asClass.isSealed
    def findChildren(tpe: Type): Seq[(String, Symbol, Int)] = tpe.typeSymbol.asClass.knownDirectSubclasses.toVector.map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2).map{ case (x, n) => (x.name.encodedName.toString, x.asType.toType.typeSymbol, n) }
    def findChildren1(tpe: Type): Seq[(Type, Int)] = tpe.typeSymbol.asClass.knownDirectSubclasses.toVector.map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2).map{ case (x, n) => (x.asType.toType, n) }

    sealed trait PursType
    final case class PursDataType(tpe: Type, children: Seq[(Type,Int)], firstLevel: Boolean) extends PursType
    final case class PursSimpleType(tpe: Type) extends PursType

    def collectTypes(tpe: Type): Seq[PursType] = {
      val complexType: Type => Boolean = {
        case tpe if tpe =:= StringClass.selfType => false
        case tpe if tpe =:= IntClass.selfType => false
        case tpe if tpe =:= BooleanClass.selfType => false
        case tpe if tpe =:= typeOf[Array[Byte]] => false
        case _ => true
      }
      @tailrec def loop(head: Type, tail: Seq[Type], acc: Seq[PursType], firstLevel: Boolean): Seq[PursType] = {
        val (tail1, acc1) =
          if (isTrait(head)) {
            val children = findChildren1(head)
            (children.map(_._1)++tail, acc:+PursDataType(head, children, firstLevel))
          } else if (head.typeConstructor =:= OptionClass.selfType.typeConstructor) {
            val typeArg = head.typeArgs.head.typeSymbol
            val typeArgType = typeArg.asType.toType
            if (complexType(typeArgType)) (typeArgType+:tail, acc)
            else (tail, acc)
          } else if (isIterable(head)) {
            val typeArg = head.typeArgs.head.typeSymbol
            val typeArgType = typeArg.asType.toType
            if (complexType(typeArgType)) (typeArgType+:tail, acc)
            else (tail, acc)
          } else {
            val fs = fields(head.typeSymbol).map(_._2).filter(complexType)
            (fs++tail, acc:+PursSimpleType(head))
          }
        tail1 match {
          case h +: t => loop(h, t, acc1, false)
          case _ => acc1
        }
      }
      loop(tpe, Nil, Nil, true)
    }

    def makeEncodePursType(x: Type): Seq[String] = makePursType(x, genMaybe=false)
    def makeDecodePursType(x: Type): Seq[String] = makePursType(x, genMaybe=true)
    def makePursType(x: Type, genMaybe: Boolean): Seq[String] = {
      collectTypes(x).flatMap{
        case PursDataType(tpe, children,_) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          s"data ${name} = ${children.map(_._1.typeSymbol.name.encodedName.toString).map(x => s"${x} ${x}").mkString(" | ")}" +: Vector.empty
        case PursSimpleType(tpe) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val fieldsOf = fields(tpe.typeSymbol)
          val fs = fieldsOf.map{ case (name1, tpe, _) =>
          val pursType =
            if (tpe =:= StringClass.selfType) {
              "String" -> "Maybe String"
            } else if (tpe =:= IntClass.selfType) {
              "Int" -> "Maybe Int"
            } else if (tpe =:= BooleanClass.selfType) {
              "Boolean" -> "Maybe Boolean"
            } else if (tpe =:= typeOf[Array[Byte]]) {
              "Uint8Array" -> "Maybe Uint8Array"
            } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
              val typeArg = tpe.typeArgs.head.typeSymbol
              val typeArgName = typeArg.asClass.name.encodedName.toString
              s"Maybe ${typeArgName}" -> s"Maybe ${typeArgName}"
            } else if (isIterable(tpe)) {
              val typeArg = tpe.typeArgs.head.typeSymbol
              val typeArgName = typeArg.asClass.name.encodedName.toString
              s"Array ${typeArgName}" -> s"Array ${typeArgName}"
            } else {
              val symbol = tpe.typeSymbol
              val name = symbol.name.encodedName.toString
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
        case PursDataType(tpe, children, true) =>
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
        case PursDataType(tpe, children, false) =>
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
        case PursSimpleType(tpe) =>
          val fs = fields1(tpe)
          val defObj: String = fs.map{
            case (name, tpe, _) =>
              if (tpe =:= StringClass.selfType) {
                s"""${name}: Nothing"""
              } else if (tpe =:= IntClass.selfType) {
                s"""${name}: Nothing"""
              } else if (tpe =:= OptionClass.selfType.typeConstructor) {
                s"""${name}: Nothing"""
              } else if (isIterable(tpe)) {
                s"${name}: []"
              } else if (isTrait(tpe)) {
                s"${name}: Nothing"
              } else {
                s"${name}: Nothing"
              }
          }.mkString("{ ", ", ", " }")
          val justObj: String = fs.map{
            case (name, tpe, _) =>
              if (tpe =:= StringClass.selfType) {
                s"""${name}: Just ${name}"""
              } else if (tpe =:= IntClass.selfType) {
                s"""${name}: Just ${name}"""
              } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
                name
              } else if (isIterable(tpe)) {
                name
              } else if (isTrait(tpe)) {
                s"${name}: Just ${name}"
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
          val cases = fields1(tpe).map{ case (name, tpe, n) =>
            if (tpe =:= StringClass.selfType) {
              decodeTmpl(n, "Decode.string", s"${name} = Just val")
            } else if (tpe =:= IntClass.selfType) {
              decodeTmpl(n, "Decode.int32", s"${name} = Just val")
            } else if (tpe =:= BooleanClass.selfType) {
              decodeTmpl(n, "Decode.boolean", s"${name} = Just val")
            } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
              val typeArg = tpe.typeArgs.head.typeSymbol
              val typeArgType = typeArg.asType.toType
              if (typeArgType =:= StringClass.selfType) {
                decodeTmpl(n, "Decode.string", s"${name} = Just val")
              } else {
                val typeArgName = typeArg.asClass.name.encodedName.toString
                decodeTmpl(n, s"decode${typeArgName}", s"${name} = Just val")
              }
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
              val symbol = tpe.typeSymbol
              val symbolName = symbol.name.encodedName.toString
              decodeTmpl(n, s"decode${symbolName}", s"${name} = Just val")
            }
          }.flatten
          val name = tpe.typeSymbol.name.encodedName.toString
          s"""|decode${name} :: Uint8Array -> Int -> Decode.Result ${name}
              |decode${name} _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  { pos: pos1, val } <- decode end ${defObj} pos
              |  case val of
              |    ${justObj} -> pure { pos: pos1, val: ${unObj} }
              |    _ -> Left $$ Decode.MissingFields "${name}"
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

    val decodeTypes = makeDecodePursType(typeOf[D])
    val decoders = makeDecoders(typeOf[D])

    def makeEncoders(tpe: Type): Seq[String] = {
      collectTypes(tpe).map{
        case PursDataType(tpe, children,_) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val cases = children.map{ case (tpe, n) =>
            val name1 = tpe.typeSymbol.name.encodedName.toString
            List(
              s"encode${name} (${name1} x) = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode${name1} x ]"
            )
          }
          s"""|encode${name} :: ${name} -> Uint8Array
              |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
        case PursSimpleType(tpe) =>
          val encodeFields = fields1(tpe).flatMap{ case (name, tpe, n) =>
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

    val encodeTypes = makeEncodePursType(typeOf[E])
    val encoders = makeEncoders(typeOf[E])

    Res(
      prelude(moduleName),
      decodeTypes.distinct,
      encodeTypes.distinct,
      decoders.distinct,
      encoders.distinct,
    )
  }

  def prelude(moduleName: String): String = s"""module ${moduleName} where

import Data.Array (snoc, concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int.Bits (zshr, (.&.))
import Prelude (bind, pure, ($$), (+), (<))
import Proto.Encode as Encode
import Proto.Decode as Decode
import Uint8ArrayExt (length, concatAll)
"""
}
