package proto
package tex

import scala.quoted.*
import scala.collection.SortedMap

case class GenRes(
    doc: String
  , argsExamples: String
  )

case class GenResRaw(
    doc: List[String]
  , argsExamples: List[String]
  ) {
    def toGenRes: GenRes =
      val doc = this.doc.mkString
      val argsExamples = this.argsExamples.mkString
      GenRes(
        doc = doc
      , argsExamples = argsExamples
      )
  }

inline def generate[D, E](
    inline category: Map[Int, String], inline ask: String, inline ok: String, inline err: String
  ): GenRes = 
    ${Macro.generate[D, E]('category, 'ask, 'ok, 'err)}

object Macro:
  def generate[D: Type, E: Type](
      category: Expr[Map[Int, String]]
    , ask: Expr[String]
    , ok: Expr[String]
    , err: Expr[String]
    )(using qctx: Quotes): Expr[GenRes] = 
    Impl().generate[D, E](category, ask, ok, err)
end Macro

private class Impl(using val qctx: Quotes) extends Ops with Doc:
  import qctx.reflect.*

  def generate[D: Type, E: Type](
      category: Expr[Map[Int, String]]
    , ask: Expr[String]
    , ok: Expr[String]
    , err: Expr[String]
    ): Expr[GenRes] =

    val tpeD = TypeRepr.of[D]
    val tpeE = TypeRepr.of[E]

    val decodeTpes = collectTpes(tpeD)
    val encodeTpes = collectTpes(tpeE)

    val commonTpes = encodeTpes intersect decodeTpes

    val childrenD = findChildren(tpeD)
    val childrenE = findChildren(tpeE)
    val messages = childrenD ++ childrenE
    val others = messages.map(_.tpe).flatMap(x => type_to_tpe(x)._1) match {
      case x +: xs =>
        collectTpes(head=x, tail=xs, acc=Nil, firstLevel=false)
      case Nil => Nil
    }

    val sorted: SortedMap[Int, String] = SortedMap.from(category.valueOrAbort.toList)
    val categoryFun: Int => String = number => {
      sorted.collectFirst{ case (n, cat) if number < n => cat }.getOrElse("???")
    }

    val doc = 
      Expr.ofList(
        tex1(messages=messages, others=others, category=categoryFun, ask=ask.valueOrAbort, ok=ok.valueOrAbort, err=err.valueOrAbort)
          .grouped(1024)
          .toList
          .map(Expr.apply)
      )
    
    val argsExamples = 
      Expr.ofList(
        examples(childrenE, commonTpes.map(_.tpe))
          .grouped(1024)
          .toList
          .map(Expr.apply)
      )

    '{
      GenResRaw(
        doc = ${doc}
      , argsExamples = ${argsExamples}
      ).toGenRes
    }
end Impl