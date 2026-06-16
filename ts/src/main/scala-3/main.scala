package proto
package ts

import scala.quoted.*
import compiletime.asMatchable

type Content = String
type ModuleName = String

case class GenResRaw(
  ts: Map[ModuleName, List[String]],
  defaultValues: Map[String, Any]
) {
  def toGenRes: GenRes =
    val ts = this.ts.map( (moduleName, contents) =>
      var content: String = contents.mkString
      defaultValues.foreach{ (id, value) =>
        val valueStr = (
          value.asMatchable match {
            case xs: List[?] => xs.mkString("")
            case other => other.toString
          }
        ).replace("Map()", "[]")
        content = content.replace(id, valueStr).nn
      }
      moduleName -> content
    )
    GenRes(ts = ts)
}

case class GenRes(
  ts: Map[ModuleName, Content]
)

/** Generate native TypeScript codecs from Scala models.
  *
  * @param moduleEncode file/module name for encoders (operates on `E`)
  * @param moduleDecode file/module name for decoders (operates on `D`)
  * @param moduleCommon file/module name for types shared by both
  * @param runtime      import specifier for the hand-written runtime (default `"./proto"`)
  */
inline def generate[D, E](
    inline moduleEncode: ModuleName
  , inline moduleDecode: ModuleName
  , inline moduleCommon: ModuleName
  , inline runtime: String = "./proto"
  ): GenRes =
    ${Macro.generate[D, E]('moduleEncode, 'moduleDecode, 'moduleCommon, 'runtime)}

object Macro:
  def generate[D: Type, E: Type](
      moduleEncodeExpr: Expr[ModuleName]
    , moduleDecodeExpr: Expr[ModuleName]
    , moduleCommonExpr: Expr[ModuleName]
    , runtimeExpr: Expr[String]
    )(using Quotes): Expr[GenRes] =
    new Impl().generate[D, E](moduleEncodeExpr, moduleDecodeExpr, moduleCommonExpr, runtimeExpr)
end Macro

private class Impl(using Quotes) extends Ops with Types with Encoders with Decoders:
  import qctx.reflect.*

  def generate[D: Type, E: Type](
      moduleEncodeExpr: Expr[ModuleName]
    , moduleDecodeExpr: Expr[ModuleName]
    , moduleCommonExpr: Expr[ModuleName]
    , runtimeExpr: Expr[String]
    ): Expr[GenRes] =

    val moduleEncode = moduleEncodeExpr.valueOrAbort
    val moduleDecode = moduleDecodeExpr.valueOrAbort
    val moduleCommon = moduleCommonExpr.valueOrAbort
    val runtime = runtimeExpr.valueOrAbort

    val tpeD = TypeRepr.of[D]
    val tpeE = TypeRepr.of[E]

    val decodeTpes = collectTpes(tpeD)
    val encodeTpes = collectTpes(tpeE)

    val variants = traitChildNames(encodeTpes ++ decodeTpes)

    val encoders = encodersFrom(encodeTpes)
    val decoders = decodersFrom(decodeTpes, variants)

    val defExpr = collectDefExpressions(tpeD) ++ collectDefExpressions(tpeE)
    val namesExpr: Expr[List[String]] = Expr.ofList(defExpr.map(_._1).toList)
    val valuesExpr: Expr[List[Any]] = Expr.ofList(defExpr.map(_._2).toList)

    val commonTpes = encodeTpes intersect decodeTpes
    val commonTsTypes = makeTsTypes(commonTpes, variants)
    val encodeTsTypes = makeTsTypes(encodeTpes, variants) diff commonTsTypes
    val decodeTsTypes = makeTsTypes(decodeTpes, variants) diff commonTsTypes

    val commonNames = commonTsTypes.flatMap(_.`export`).distinct
    val importCommon =
      if commonNames.nonEmpty then s"""import { ${commonNames.mkString(", ")} } from "./$moduleCommon"\n"""
      else ""

    val moduleCommonValue =
      if commonTsTypes.nonEmpty then commonTsTypes.flatMap(_.tmpl).mkString("\n\n")
      else ""

    val moduleEncodeValue =
      val decls = encodeTsTypes.flatMap(_.tmpl).mkString("\n\n")
      val funs = encoders.map(_.tmpl).mkString("\n\n")
      s"""|import { Encode } from "$runtime"
          |$importCommon
          |$decls${if decls.nonEmpty then "\n\n" else ""}$funs""".stripMargin

    val moduleDecodeValue =
      val decls = decodeTsTypes.flatMap(_.tmpl).mkString("\n\n")
      val funs = decoders.map(_.tmpl).mkString("\n\n")
      s"""|import { Decode } from "$runtime"
          |$importCommon
          |$decls${if decls.nonEmpty then "\n\n" else ""}$funs""".stripMargin

    val strToContentExpr: String => Expr[List[String]] = str => Expr.ofList(str.grouped(1024).toList.map(Expr.apply))

    '{
      GenResRaw(
        ts = Map(
            ${moduleCommonExpr} -> ${strToContentExpr(moduleCommonValue)}
          , ${moduleEncodeExpr} -> ${strToContentExpr(moduleEncodeValue)}
          , ${moduleDecodeExpr} -> ${strToContentExpr(moduleDecodeValue)}
        ),
        defaultValues = $namesExpr.zip($valuesExpr).toMap
      ).toGenRes
    }

end Impl
