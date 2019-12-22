package zd.proto.purs

import scala.reflect.runtime.universe._
import zd.proto.api.MessageCodec

object Purescript {
  type ModuleName = String
  type Content = String
  def generate[D, E](moduleEncode: ModuleName, moduleDecode: ModuleName, moduleCommon: ModuleName, codecs: List[MessageCodec[_]])(implicit dtag: TypeTag[D], etag: TypeTag[E]): List[(ModuleName, Content)] = {
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
