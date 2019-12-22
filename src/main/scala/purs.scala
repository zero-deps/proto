package zd.proto.purs

import scala.reflect.runtime.universe._
import zd.proto.api.MessageCodec
import zd.gs.z._

object Purescript {
  type ModuleName = String
  type Content = String
  def generate[D, E](moduleEncode: ModuleName, moduleDecode: ModuleName, moduleCommon: ModuleName, codecs: List[MessageCodec[_]])(implicit dtag: TypeTag[D], etag: TypeTag[E]): List[(ModuleName, Content)] = {

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
          val fs = fields(tpe).map{ case (name1, tpe, _) => name1 -> pursType(tpe) }
          val params = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._1}" }.mkString(", ")
          val x = s"newtype $name = $name { $params }"
          if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._2}" }.mkString(", ")
            if (params == params1) {
              Seq(PursType(Seq(x), s"$name($name)".just))
            } else {
              val x1 = s"newtype $name' = $name' { $params1 }"
              Seq(PursType(Seq(x), s"$name($name)".just), PursType(Seq(x1), Nothing))
            }
          } else {
            Seq(PursType(Seq(x), s"$name($name)".just))
          }
        case RegularType(tpe) =>
          val name = tpe.typeSymbol.name.encodedName.toString
          val fs = fields(tpe).map{ case (name1, tpe, _) => name1 -> pursType(tpe) }
          val params = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._1}" }.mkString(", ")
          val x = if (params.nonEmpty) s"type $name = { $params }" else s"type $name = {}"
          if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._2}" }.mkString(", ")
            if (params == params1) {
              Seq(PursType(Seq(x), s"$name".just))
            } else {
              val x1 = s"type $name' = { $params1 }"
              Seq(PursType(Seq(x), s"$name".just), PursType(Seq(x1), Nothing))
            }
          } else {
            Seq(PursType(Seq(x), s"$name".just))
          }
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

    List(
      moduleCommon ->
        s"""|module $moduleCommon
            |  ( ${commonPursTypes.flatMap(_.export).mkString("\n  , ")} 
            |  ) where
            |
            |import Data.Eq (class Eq)
            |import Data.Maybe (Maybe)
            |import Data.Tuple (Tuple)
            |
            |${commonPursTypes.flatMap(_.tmpl).mkString("\n")}""".stripMargin
    , moduleEncode ->
        s"""|module $moduleEncode
            |  ( ${(encodePursTypes.flatMap(_.export)++encoders.flatMap(_.export)).mkString("\n  , ")}
            |  ) where
            |
            |import Data.Array (concatMap)
            |import Data.ArrayBuffer.Types (Uint8Array)
            |import Data.Eq (class Eq)
            |import Data.Maybe (Maybe, fromMaybe)
            |import Data.Tuple (Tuple(Tuple))
            |import Prelude (map, ($$))
            |import Proto.Encode as Encode
            |import Proto.Uint8ArrayExt (length, concatAll, fromArray)
            |import $moduleCommon
            |
            |${encodePursTypes.flatMap(_.tmpl).mkString("\n")}
            |
            |${encoders.map(_.tmpl).mkString("\n\n")}""".stripMargin
    , moduleDecode ->
        s"""|module $moduleDecode
            |  ( ${(decodePursTypes.flatMap(_.export)++decoders.flatMap(_.export)).mkString("\n  , ")}
            |  ) where
            |
            |import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
            |import Data.Array (snoc)
            |import Data.ArrayBuffer.Types (Uint8Array)
            |import Data.Either (Either(Left))
            |import Data.Eq (class Eq)
            |import Data.Int.Bits (zshr, (.&.))
            |import Data.Maybe (Maybe(Just, Nothing))
            |import Data.Tuple (Tuple(Tuple))
            |import Data.Unit (Unit, unit)
            |import Prelude (map, bind, pure, ($$), (+), (<), (<<<))
            |import Proto.Decode as Decode
            |import $moduleCommon
            |
            |decodeField :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
            |decodeField end res f = map (\\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res
            |
            |${decodePursTypes.flatMap(_.tmpl).mkString("\n")}
            |
            |${decoders.map(_.tmpl).mkString("\n\n")}""".stripMargin
    )
  }
}
