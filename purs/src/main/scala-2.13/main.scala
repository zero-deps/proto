package proto
package purs

import scala.reflect.runtime.universe._

import Ops._

object Purescript {
  final case class GenRes(
    purs: Map[ModuleName, Content]
  )
  type ModuleName = String
  type Content = String
  def generate[D, E](
      moduleEncode: ModuleName
    , moduleDecode: ModuleName
    , moduleCommon: ModuleName
  )(
    implicit dtag: TypeTag[D], etag: TypeTag[E]
  ): GenRes = {
    val decodeTpes = collectTpes(typeOf[D])
    val decoders = Decoders.from(decodeTpes)

    val encodeTpes = collectTpes(typeOf[E])
    val encoders = Encoders.from(encodeTpes)

    val commonTpes = encodeTpes intersect decodeTpes
    val commonPursTypes = makePursTypes(commonTpes, genMaybe=false)
    val decodePursTypes = makePursTypes(decodeTpes, genMaybe=true) diff commonPursTypes
    val encodePursTypes = makePursTypes(encodeTpes diff commonTpes, genMaybe=false)

    GenRes(
      Map(
        moduleCommon -> {
          if (commonPursTypes.nonEmpty) {
            val code = commonPursTypes.flatMap(_.tmpl).mkString("\n")
            val is = List(
              if (code contains " :: Eq ") Some("Data.Eq" -> "class Eq") else None
            , if (code contains "Nothing") Some("Data.Maybe" -> "Maybe(Nothing)") else None
            , if (code contains "Tuple ") Some("Data.Tuple" -> "Tuple") else None
            , if (code contains "BigInt ") Some("Proto.BigInt" -> "BigInt") else None
            ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + " (" + x._2 + ")").to(List).sorted.mkString("\n")
            s"""|module $moduleCommon
                |  ( ${commonPursTypes.flatMap(_.export).mkString("\n  , ")} 
                |  ) where
                |
                |$is
                |
                |$code""".stripMargin
          } else ""
        }
      , moduleEncode -> {
          val code = encodePursTypes.flatMap(_.tmpl).mkString("\n") + "\n\n" + encoders.map(_.tmpl).mkString("\n\n")
          val is = List(
            if (code contains "concatMap ") Some("Data.Array" -> "concatMap") else None
          , if (code contains "Uint8Array") Some("Proto.Uint8Array" -> "Uint8Array") else None
          , if (code contains "BigInt") Some("Proto.BigInt" -> "BigInt") else None
          , if (code contains " :: Eq ") Some("Data.Eq" -> "class Eq") else None
          , if (raw"\WMaybe ".r.findFirstIn(code).isDefined) Some("Data.Maybe" -> "Maybe(..)") else None
          // , if (code contains "Nothing") Some("Data.Maybe" -> "Maybe(Nothing)") else None
          , if (code contains "fromMaybe ") Some("Data.Maybe" -> "fromMaybe") else None
          , if (code contains "Tuple ") Some("Data.Tuple" -> "Tuple(Tuple)") else None
          , if (code contains "map ") Some("Prelude" -> "map") else None
          , if (code contains " $ ") Some("Prelude" -> "($)") else None
          , if (code contains "Encode.") Some("Proto.Encode as Encode" -> "") else None
          , if (code contains "length ") Some("Proto.Uint8Array" -> "length") else None
          , if (code contains "concatAll ") Some("Proto.Uint8Array" -> "concatAll") else None
          , if (code contains "fromArray ") Some("Proto.Uint8Array" -> "fromArray") else None
          ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + Some(x._2).filter(_.nonEmpty).map(" ("+_+")").getOrElse("")).to(List).sorted.mkString("\n")
          s"""|module $moduleEncode
              |  ( ${(encodePursTypes.flatMap(_.export)++encoders.flatMap(_.export)).mkString("\n  , ")}
              |  ) where
              |
              |$is
              |${if (commonPursTypes.nonEmpty) s"import $moduleCommon" else ""}
              |
              |$code""".stripMargin
        }
      , moduleDecode -> {
          val code =
            s"""|decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
                |decodeFieldLoop end res f = map (\\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res""".stripMargin + "\n\n" + decodePursTypes.flatMap(_.tmpl).mkString("\n") + "\n\n" + decoders.map(_.tmpl).mkString("\n\n")
          val is = List(
            if (code.contains("Loop ") || code.contains("Done ")) Some("Control.Monad.Rec.Class" -> "Step(Loop, Done)") else None
          , if (code contains "tailRecM3 ") Some("Control.Monad.Rec.Class" -> "tailRecM3") else None
          , if (code contains "snoc ") Some("Data.Array" -> "snoc") else None
          , if (code contains "Uint8Array ") Some("Proto.Uint8Array" -> "Uint8Array") else None
          , if (code contains "BigInt") Some("Proto.BigInt" -> "BigInt") else None
          , if (code contains "Left ") Some("Data.Either" -> "Either(Left)") else None
          , if (code contains " :: Eq ") Some("Data.Eq" -> "class Eq") else None
          , if (code contains "zshr") Some("Data.Int.Bits" -> "zshr") else None
          , if (code contains " .&. ") Some("Data.Int.Bits" -> "(.&.)") else None
          , if (code.contains("Just ") || code.contains("Nothing")) Some("Data.Maybe" -> "Maybe(Just, Nothing)") else None
          , if (code contains "fromMaybe ") Some("Data.Maybe" -> "fromMaybe") else None
          , if (code contains "Tuple ") Some("Data.Tuple" -> "Tuple(Tuple)") else None
          , if (code contains "Unit") Some("Data.Unit" -> "Unit") else None
          , if (code contains "unit") Some("Data.Unit" -> "unit") else None
          , if (code contains "map ") Some("Prelude" -> "map") else None
          , if (code contains " do") Some("Prelude" -> "bind") else None
          , if (code contains "pure ") Some("Prelude" -> "pure") else None
          , if (code contains " $ ") Some("Prelude" -> "($)") else None
          , if (code contains " + ") Some("Prelude" -> "(+)") else None
          , if (code contains " < ") Some("Prelude" -> "(<)") else None
          , if (code contains " <<< ") Some("Prelude" -> "(<<<)") else None
          , if (code contains "Decode.") Some("Proto.Decode as Decode" -> "") else None
          ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + Some(x._2).filter(_.nonEmpty).map(" ("+_+")").getOrElse("")).to(List).sorted.mkString("\n")
          s"""|module $moduleDecode
              |  ( ${(decodePursTypes.flatMap(_.export)++decoders.flatMap(_.export)).mkString("\n  , ")}
              |  ) where
              |
              |$is
              |${if (commonPursTypes.nonEmpty) s"import $moduleCommon" else ""}
              |
              |$code""".stripMargin
        }
      )
    )
  }
}
