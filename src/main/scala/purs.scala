package zd.proto.purs

import scala.reflect.runtime.universe._
import zd.proto.api.MessageCodec
import zd.gs.z._

final case class Coder(tmpl: String, export: Maybe[String])

final case class Res2(common: Res, encode: Res, decode: Res)
final case class Res(prelude: String, types: Seq[PursType], coders: Seq[Coder])
object Res {
  def format(res: Res): String = {
    Seq(res.prelude, res.types.flatMap(_.tmpl).mkString("\n"), res.coders.map(_.tmpl).mkString("\n\n")).mkString("\n\n")
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
    def preludeCommon(exports: Seq[String]): String = s"""module $commonModule
${exports.mkString("  ( ", "\n  , ", "\n  )")} where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)"""

    def preludeEncode(exports: Seq[String]): String = s"""module $moduleEncodeName
${exports.mkString("  ( ", "\n  , ", "\n  )")} where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Eq (class Eq)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, ($$))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll, fromArray)
import $commonModule"""

    def preludeDecode(exports: Seq[String]): String = s"""module $moduleDecodeName
${exports.mkString("  ( ", "\n  , ", "\n  )")} where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit, unit)
import Prelude (bind, pure, ($$), (+), (<))
import Proto.Decode as Decode
import $commonModule"""

    def makePursTypes(types: Seq[Tpe], genMaybe: Boolean): Seq[PursType] = {
      types.flatMap{
        case TraitType(tpe, children, true) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          List(PursType(List(
            s"data $name = ${children.map{
              case x if x.noargs => x.name
              case x => s"${x.name} ${x.name}"
            }.mkString(" | ")}"
          ), export=s"$name(..)".just))
        case TraitType(tpe, children, false) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          List(PursType(List(
            s"data $name = ${children.map{
              case x if x.noargs => x.name
              case x => s"${x.name} ${x.name}"
            }.mkString(" | ")}"
          , s"derive instance eq$name :: Eq $name"
          ), export=s"$name(..)".just))
        case _: TupleType => Nil
        case _: NoargsType => Nil
        case RecursiveType(tpe) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val fieldsOf = fields(tpe)
          val fs = fieldsOf.map{ case (name1, tpe, _) => name1 -> pursType(tpe) }
          val x = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._1}" }.mkString(s"newtype $name = $name { ", ", ", " }")
          val x1 = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._2}" }.mkString(s"newtype $name' = $name' { ", ", ", " }")
          if (genMaybe) Seq(PursType(Vector(x), s"$name($name)".just), PursType(Seq(x1), Nothing)) else Seq(PursType(Vector(x), s"$name($name)".just))
        case RegularType(tpe) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val fieldsOf = fields(tpe)
          val fs = fieldsOf.map{ case (name1, tpe, _) => name1 -> pursType(tpe) }
          val x = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._1}" }.mkString(s"type $name = { ", ", ", " }")
          val x1 = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._2}" }.mkString(s"type $name' = { ", ", ", " }")
          if (genMaybe) Seq(PursType(Vector(x), s"$name".just), PursType(Seq(x1), Nothing)) else Seq(PursType(Vector(x), s"$name".just))
      }.distinct
    }

    val decodeTpes = collectTpes(typeOf[D])
    val decoders = Decoders.from(decodeTpes, codecs)

    val encodeTpes = collectTpes(typeOf[E])
    val encoders = Encoders.from(encodeTpes, codecs)

    val commonTpes = encodeTpes intersect decodeTpes
    val commonPursTypes = makePursTypes(commonTpes, genMaybe=false)
    val decodePursTypes = makePursTypes(decodeTpes, genMaybe=true) diff commonPursTypes
    val encodePursTypes = makePursTypes(encodeTpes diff commonTpes, genMaybe=false)

    Res2(
      common=Res(preludeCommon(exports=commonPursTypes.flatMap(_.export)), commonPursTypes, Nil)
    , encode=Res(preludeEncode(exports=encodePursTypes.flatMap(_.export)++encoders.flatMap(_.export)), encodePursTypes, encoders)
    , decode=Res(preludeDecode(exports=decodePursTypes.flatMap(_.export)++decoders.flatMap(_.export)), decodePursTypes, decoders)
    )
  }
}
