package proto
package tex

import scala.annotation._
import scala.reflect.runtime.universe._

import Ops._

object Run {
  final case class GenRes(
    doc: (Content, Doc.ChangeLog)
  )
  type ModuleName = String
  type Content = String
  def generate[D, E](
      category: Int => String
    , ask: String, ok: String, err: String
  )(
    implicit dtag: TypeTag[D], etag: TypeTag[E]
  ): GenRes = {
    val decodeTpes = collectTpes(typeOf[D])
    val encodeTpes = collectTpes(typeOf[E])
    GenRes(
      {
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
