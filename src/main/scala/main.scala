package zd.proto.purs

import scala.reflect.runtime.universe._
import zd.proto.api.MessageCodec
import zd.gs.z._

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
      moduleCommon -> {
        val code = commonPursTypes.flatMap(_.tmpl).mkString("\n")
        val is = List(
          if (code contains " :: Eq ") ("Data.Eq" -> "class Eq").just else Nothing
        , if (code contains "Nullable ") ("Data.Nullable" -> "Nullable").just else Nothing
        , if (code contains "Tuple ") ("Data.Tuple" -> "Tuple").just else Nothing
        ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + " (" + x._2 + ")").to(List).sorted.mkString("\n")
        s"""|module $moduleCommon
            |  ( ${commonPursTypes.flatMap(_.export).mkString("\n  , ")} 
            |  ) where
            |
            |$is
            |
            |$code""".stripMargin
      }
    , moduleEncode -> {
        val code = encodePursTypes.flatMap(_.tmpl).mkString("\n") + "\n\n" + encoders.map(_.tmpl).mkString("\n\n")
        val is = List(
          if (code contains "concatMap ") ("Data.Array" -> "concatMap").just else Nothing
        , if (code contains "Uint8Array") ("Data.ArrayBuffer.Types" -> "Uint8Array").just else Nothing
        , if (code contains " :: Eq ") ("Data.Eq" -> "class Eq").just else Nothing
        , if (code contains "Nullable ") ("Data.Nullable" -> "Nullable").just else Nothing
        , if (code contains "nullable ") ("Data.Nullable" -> "nullable").just else Nothing
        , if (code contains "Tuple ") ("Data.Tuple" -> "Tuple(Tuple)").just else Nothing
        , if (code contains "map ") ("Prelude" -> "map").just else Nothing
        , if (code contains " $ ") ("Prelude" -> "($)").just else Nothing
        , if (code contains "Encode.") ("Proto.Encode as Encode" -> "").just else Nothing
        , if (code contains "length ") ("Proto.Uint8ArrayExt" -> "length").just else Nothing
        , if (code contains "concatAll ") ("Proto.Uint8ArrayExt" -> "concatAll").just else Nothing
        , if (code contains "fromArray ") ("Proto.Uint8ArrayExt" -> "fromArray").just else Nothing
        ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + x._2.just.filter(_.nonEmpty).map(" ("+_+")").getOrElse("")).to(List).sorted.mkString("\n")
        s"""|module $moduleEncode
            |  ( ${(encodePursTypes.flatMap(_.export)++encoders.flatMap(_.export)).mkString("\n  , ")}
            |  ) where
            |
            |$is
            |import $moduleCommon
            |
            |$code""".stripMargin
      }
    , moduleDecode -> {
        val code =
          s"""|decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
              |decodeFieldLoop end res f = map (\\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res""".stripMargin + "\n\n" + decodePursTypes.flatMap(_.tmpl).mkString("\n") + "\n\n" + decoders.map(_.tmpl).mkString("\n\n")
        val is = List(
          if (code.contains("Loop ") || code.contains("Done ")) ("Control.Monad.Rec.Class" -> "Step(Loop, Done)").just else Nothing
        , if (code contains "tailRecM3 ") ("Control.Monad.Rec.Class" -> "tailRecM3").just else Nothing
        , if (code contains "snoc ") ("Data.Array" -> "snoc").just else Nothing
        , if (code contains "Uint8Array ") ("Data.ArrayBuffer.Types" -> "Uint8Array").just else Nothing
        , if (code contains "Left ") ("Data.Either" -> "Either(Left)").just else Nothing
        , if (code contains " :: Eq ") ("Data.Eq" -> "class Eq").just else Nothing
        , if (code contains "zshr") ("Data.Int.Bits" -> "zshr").just else Nothing
        , if (code contains " .&. ") ("Data.Int.Bits" -> "(.&.)").just else Nothing
        , if (code.contains("notNull ")) ("Data.Nullable" -> "notNull").just else Nothing
        , if (code.contains("null")) ("Data.Nullable" -> "null").just else Nothing
        , if (code contains "Tuple ") ("Data.Tuple" -> "Tuple(Tuple)").just else Nothing
        , if (code contains "Unit") ("Data.Unit" -> "Unit").just else Nothing
        , if (code contains "unit") ("Data.Unit" -> "unit").just else Nothing
        , if (code contains "map ") ("Prelude" -> "map").just else Nothing
        , if (code contains " do") ("Prelude" -> "bind").just else Nothing
        , if (code contains "pure ") ("Prelude" -> "pure").just else Nothing
        , if (code contains " $ ") ("Prelude" -> "($)").just else Nothing
        , if (code contains " + ") ("Prelude" -> "(+)").just else Nothing
        , if (code contains " < ") ("Prelude" -> "(<)").just else Nothing
        , if (code contains " <<< ") ("Prelude" -> "(<<<)").just else Nothing
        , if (code contains "Decode.") ("Proto.Decode as Decode" -> "").just else Nothing
        ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + x._2.just.filter(_.nonEmpty).map(" ("+_+")").getOrElse("")).to(List).sorted.mkString("\n")
        s"""|module $moduleDecode
            |  ( ${(decodePursTypes.flatMap(_.export)++decoders.flatMap(_.export)).mkString("\n  , ")}
            |  ) where
            |
            |$is
            |import $moduleCommon
            |
            |$code""".stripMargin
      }
    )
  }
}
