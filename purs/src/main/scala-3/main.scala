package proto
package purs

import scala.quoted.*

type Content = String
type ModuleName = String

case class GenResRaw(
  purs: Map[ModuleName, List[String]],
  defaultValues: Map[String, Any]
) {
  def toGenRes: GenRes =
    val purs = this.purs.map( (moduleName, contents) =>
      var content: String = contents.mkString
      defaultValues.foreach{ (id, value) =>
        content = content.replace(id, value.toString).nn
      }
      moduleName -> content
    )
    GenRes(purs = purs)
}

case class GenRes(
  purs: Map[ModuleName, Content]
)

inline def generate[D, E](
    inline moduleEncode: ModuleName
  , inline moduleDecode: ModuleName
  , inline moduleCommon: ModuleName
  ): GenRes = 
    ${Macro.generate[D, E]('moduleEncode, 'moduleDecode, 'moduleCommon)}

object Macro:
  def generate[D: Type, E: Type](
      moduleEncodeExpr: Expr[ModuleName]
    , moduleDecodeExpr: Expr[ModuleName]
    , moduleCommonExpr: Expr[ModuleName]
    )(using qctx: Quotes): Expr[GenRes] = 
    Impl().generate[D, E](moduleEncodeExpr, moduleDecodeExpr, moduleCommonExpr)
end Macro

private class Impl(using val qctx: Quotes) extends Ops with Encoders with Decoders:
  import qctx.reflect.*

  def generate[D: Type, E: Type](
      moduleEncodeExpr: Expr[ModuleName]
    , moduleDecodeExpr: Expr[ModuleName]
    , moduleCommonExpr: Expr[ModuleName]
    ): Expr[GenRes] =

    val moduleEncode = moduleEncodeExpr.valueOrAbort
    val moduleDecode = moduleDecodeExpr.valueOrAbort
    val moduleCommon = moduleCommonExpr.valueOrAbort

    val tpeD = TypeRepr.of[D]
    val tpeE = TypeRepr.of[E]

    val decodeTpes = collectTpes(tpeD)
    val decoders = decodersFrom(decodeTpes)

    val defExpr = collectDefExpressions(tpeD) ++ collectDefExpressions(tpeE)
    val namesExpr: Expr[List[String]] = Expr.ofList(defExpr.map(_._1).toList)
    val valuesExpr: Expr[List[Any]] = Expr.ofList(defExpr.map(_._2).toList)

    val encodeTpes = collectTpes(tpeE)
    val encoders = encodersFrom(encodeTpes)

    val commonTpes = encodeTpes intersect decodeTpes
    val commonPursTypes = makePursTypes(commonTpes, genMaybe=false)
    val decodePursTypes = makePursTypes(decodeTpes, genMaybe=true) diff commonPursTypes
    val encodePursTypes = makePursTypes(encodeTpes diff commonTpes, genMaybe=false)

    val moduleCommonValue = {
      if (commonPursTypes.nonEmpty) {
        val code = commonPursTypes.flatMap(_.tmpl).mkString("\n")
        val is = List(
          if (code.contains(" :: Eq ")) Some("Data.Eq" -> "class Eq") else None
        , if (code.contains("Nothing")) Some("Data.Maybe" -> "Maybe(Nothing)") else None
        , if (code.contains("Tuple ")) Some("Data.Tuple" -> "Tuple") else None
        , if (code.contains("BigInt ")) Some("Proto.BigInt" -> "BigInt") else None
        ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + " (" + x._2 + ")").to(List).sorted.mkString("\n")
        s"""|module $moduleCommon
            |  ( ${commonPursTypes.flatMap(_.`export`).mkString("\n  , ")} 
            |  ) where
            |
            |$is
            |
            |$code""".stripMargin
      } else ""
    }
    val moduleEncodeValue = {
      val code = encodePursTypes.flatMap(_.tmpl).mkString("\n") + "\n\n" + encoders.map(_.tmpl).mkString("\n\n")
      val is = List(
        if (code.contains("concatMap ")) Some("Data.Array" -> "concatMap") else None
      , if (code.contains("Uint8Array")) Some("Proto.Uint8Array" -> "Uint8Array") else None
      , if (code.contains("BigInt")) Some("Proto.BigInt" -> "BigInt") else None
      , if (code.contains(" :: Eq ")) Some("Data.Eq" -> "class Eq") else None
      , if (raw"\WMaybe ".r.findFirstIn(code).isDefined) Some("Data.Maybe" -> "Maybe(..)") else None
      // , if (code contains "Nothing") Some("Data.Maybe" -> "Maybe(Nothing)") else None
      , if (code.contains("fromMaybe ")) Some("Data.Maybe" -> "fromMaybe") else None
      , if (code.contains("Tuple ")) Some("Data.Tuple" -> "Tuple(Tuple)") else None
      , if (code.contains("map ")) Some("Prelude" -> "map") else None
      , if (code.contains(" $ ")) Some("Prelude" -> "($)") else None
      , if (code.contains("Encode.")) Some("Proto.Encode as Encode" -> "") else None
      , if (code.contains("length ")) Some("Proto.Uint8Array" -> "length") else None
      , if (code.contains("concatAll ")) Some("Proto.Uint8Array" -> "concatAll") else None
      , if (code.contains("fromArray ")) Some("Proto.Uint8Array" -> "fromArray") else None
      ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + Some(x._2).filter(_.nonEmpty).map(" ("+_+")").getOrElse("")).to(List).sorted.mkString("\n")
      s"""|module $moduleEncode
          |  ( ${(encodePursTypes.flatMap(_.`export`)++encoders.flatMap(_.`export`)).mkString("\n  , ")}
          |  ) where
          |
          |$is
          |${if (commonPursTypes.nonEmpty) s"import $moduleCommon" else ""}
          |
          |$code""".stripMargin
    }

    val moduleDecodeValue = {
      val code =
        s"""|decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
            |decodeFieldLoop end res f = map (\\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res""".stripMargin + "\n\n" + decodePursTypes.flatMap(_.tmpl).mkString("\n") + "\n\n" + decoders.map(_.tmpl).mkString("\n\n")
      val is = List(
        if (code.contains("Loop ") || code.contains("Done ")) Some("Control.Monad.Rec.Class" -> "Step(Loop, Done)") else None
      , if (code.contains("tailRecM3 ")) Some("Control.Monad.Rec.Class" -> "tailRecM3") else None
      , if (code.contains("snoc ")) Some("Data.Array" -> "snoc") else None
      , if (code.contains("Uint8Array ")) Some("Proto.Uint8Array" -> "Uint8Array") else None
      , if (code.contains("BigInt")) Some("Proto.BigInt" -> "BigInt") else None
      , if (code.contains("Left ")) Some("Data.Either" -> "Either(Left)") else None
      , if (code.contains(" :: Eq ")) Some("Data.Eq" -> "class Eq") else None
      , if (code.contains("zshr")) Some("Data.Int.Bits" -> "zshr") else None
      , if (code.contains(" .&. ")) Some("Data.Int.Bits" -> "(.&.)") else None
      , if (code.contains("Just ") || code.contains("Nothing")) Some("Data.Maybe" -> "Maybe(Just, Nothing)") else None
      , if (code.contains("fromMaybe ")) Some("Data.Maybe" -> "fromMaybe") else None
      , if (code.contains("Tuple ")) Some("Data.Tuple" -> "Tuple(Tuple)") else None
      , if (code.contains("Unit")) Some("Data.Unit" -> "Unit") else None
      , if (code.contains("unit")) Some("Data.Unit" -> "unit") else None
      , if (code.contains("map ")) Some("Prelude" -> "map") else None
      , if (code.contains(" do")) Some("Prelude" -> "bind") else None
      , if (code.contains("pure ")) Some("Prelude" -> "pure") else None
      , if (code.contains(" $ ")) Some("Prelude" -> "($)") else None
      , if (code.contains(" + ")) Some("Prelude" -> "(+)") else None
      , if (code.contains(" < ")) Some("Prelude" -> "(<)") else None
      , if (code.contains(" <<< ")) Some("Prelude" -> "(<<<)") else None
      , if (code.contains("Decode.")) Some("Proto.Decode as Decode" -> "") else None
      ).flatten.groupMapReduce(_._1)(_._2)(_ + ", " + _).map(x => "import " + x._1 + Some(x._2).filter(_.nonEmpty).map(" ("+_+")").getOrElse("")).to(List).sorted.mkString("\n")
      s"""|module $moduleDecode
          |  ( ${(decodePursTypes.flatMap(_.`export`)++decoders.flatMap(_.`export`)).mkString("\n  , ")}
          |  ) where
          |
          |$is
          |${if (commonPursTypes.nonEmpty) s"import $moduleCommon" else ""}
          |
          |$code""".stripMargin
    }

    val strToContentExpr: String => Expr[List[String]] = str => Expr.ofList(str.grouped(1024).toList.map(Expr.apply))

    '{
      GenResRaw(
        purs = Map(
            ${moduleCommonExpr} -> ${strToContentExpr(moduleCommonValue)}
          , ${moduleEncodeExpr} -> ${strToContentExpr(moduleEncodeValue)}
          , ${moduleDecodeExpr} -> ${strToContentExpr(moduleDecodeValue)}
        ),
        defaultValues = $namesExpr.zip($valuesExpr).toMap
      ).toGenRes
    }

end Impl