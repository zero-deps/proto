package purs

import scala.reflect.runtime.universe._
import zero.ext._, option._

object Purescript {
  final case class GenRes(
    purs: Map[ModuleName, Content]
  , doc: (Content, Doc.ChangeLog)
  )
  type ModuleName = String
  type Content = String
  def generate[D, E](
      moduleEncode: ModuleName
    , moduleDecode: ModuleName
    , moduleCommon: ModuleName
    , category: Int => String
    , ask: String, ok: String, err: String
  )(implicit dtag: TypeTag[D], etag: TypeTag[E]): GenRes = {
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
              if (code contains " :: Eq ") ("Data.Eq" -> "class Eq").some else None
            , if (code contains "Nothing") ("Data.Maybe" -> "Maybe(Nothing)").some else None
            , if (code contains "Tuple ") ("Data.Tuple" -> "Tuple").some else None
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
            if (code contains "concatMap ") ("Data.Array" -> "concatMap").some else None
          , if (code contains "Uint8Array") ("Proto.Uint8Array" -> "Uint8Array").some else None
          , if (code contains " :: Eq ") ("Data.Eq" -> "class Eq").some else None
          , if (raw"\WMaybe ".r.findFirstIn(code).isDefined) ("Data.Maybe" -> "Maybe(..)").some else None
          // , if (code contains "Nothing") ("Data.Maybe" -> "Maybe(Nothing)").some else None
          , if (code contains "fromMaybe ") ("Data.Maybe" -> "fromMaybe").some else None
          , if (code contains "Tuple ") ("Data.Tuple" -> "Tuple(Tuple)").some else None
          , if (code contains "map ") ("Prelude" -> "map").some else None
          , if (code contains " $ ") ("Prelude" -> "($)").some else None
          , if (code contains "Encode.") ("Proto.Encode as Encode" -> "").some else None
          , if (code contains "length ") ("Proto.Uint8Array" -> "length").some else None
          , if (code contains "concatAll ") ("Proto.Uint8Array" -> "concatAll").some else None
          , if (code contains "fromArray ") ("Proto.Uint8Array" -> "fromArray").some else None
          ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + x._2.some.filter(_.nonEmpty).map(" ("+_+")").getOrElse("")).to(List).sorted.mkString("\n")
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
            if (code.contains("Loop ") || code.contains("Done ")) ("Control.Monad.Rec.Class" -> "Step(Loop, Done)").some else None
          , if (code contains "tailRecM3 ") ("Control.Monad.Rec.Class" -> "tailRecM3").some else None
          , if (code contains "snoc ") ("Data.Array" -> "snoc").some else None
          , if (code contains "Uint8Array ") ("Proto.Uint8Array" -> "Uint8Array").some else None
          , if (code contains "Left ") ("Data.Either" -> "Either(Left)").some else None
          , if (code contains " :: Eq ") ("Data.Eq" -> "class Eq").some else None
          , if (code contains "zshr") ("Data.Int.Bits" -> "zshr").some else None
          , if (code contains " .&. ") ("Data.Int.Bits" -> "(.&.)").some else None
          , if (code.contains("Just ") || code.contains("Nothing")) ("Data.Maybe" -> "Maybe(Just, Nothing)").some else None
          , if (code contains "fromMaybe ") ("Data.Maybe" -> "fromMaybe").some else None
          , if (code contains "Tuple ") ("Data.Tuple" -> "Tuple(Tuple)").some else None
          , if (code contains "Unit") ("Data.Unit" -> "Unit").some else None
          , if (code contains "unit") ("Data.Unit" -> "unit").some else None
          , if (code contains "map ") ("Prelude" -> "map").some else None
          , if (code contains " do") ("Prelude" -> "bind").some else None
          , if (code contains "pure ") ("Prelude" -> "pure").some else None
          , if (code contains " $ ") ("Prelude" -> "($)").some else None
          , if (code contains " + ") ("Prelude" -> "(+)").some else None
          , if (code contains " < ") ("Prelude" -> "(<)").some else None
          , if (code contains " <<< ") ("Prelude" -> "(<<<)").some else None
          , if (code contains "Decode.") ("Proto.Decode as Decode" -> "").some else None
          ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + x._2.some.filter(_.nonEmpty).map(" ("+_+")").getOrElse("")).to(List).sorted.mkString("\n")
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
      , {
          val messages = findChildren(typeOf[D]) ++ findChildren(typeOf[E])
          val others = messages.map(_.tpe).flatMap(x => type_to_tpe(x)._1) match {
            case x +: xs =>
              collectTpes(head=x, tail=xs, acc=Nil, firstLevel=false)
            case Nil => Nil
          }
          val all = (decodeTpes++encodeTpes).distinct
          Doc.tex(messages=messages, others=others, all=all, category=category, ask=ask, ok=ok, err=err)
        }
    )
  }
}
