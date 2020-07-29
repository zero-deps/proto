package zero.protopurs

import scala.reflect.runtime.universe._
import zero.ext._, option._

object Doc {
  type Version = String
  type Change = String
  type Message = String
  type ChangeLog = List[(Version, List[(Message, Change)])]
  def tex(xs: Seq[Tpe]): (String, ChangeLog) = {
    val changeLog = xs.to(List).flatMap{ x =>
      val message = x.name
      since(x.tpe.typeSymbol).map{ case (version, change) => version -> (message -> change) }
    }.groupBy(_._1).view.mapValues(_.map(_._2)).to(List)
    xs.flatMap{
      case x: TraitType if x.firstLevel => None
      case x: TraitType =>
        val a = x.children.map(y => s"  \\item ${y.name}").mkString("\n")
        val b = s"\\paragraph{${x.name}}\n" + (if (a.nonEmpty) s"\\begin{itemize}\n$a\n\\end{itemize}" else throw new Exception(s"no children: check @N on children for ${x.name}"))
        b.some
      case x @ (_: RegularType | _: RecursiveType | _: NoargsType) =>
        val a = fields(x.tpe).map(y => s"  \\item[${y._1}] ${pursTypePars(y._2)._1}").mkString("\n")
        val b = if (a.nonEmpty) s"\\paragraph{${x.name}}\n\\begin{description}\n${a}\n\\end{description}" else s"\\paragraph{${x.name}} No fields"
        b.some
      case x: TupleType => None
    }.mkString("\n") -> changeLog
  }
  
  private[this] def since(x: Symbol): List[(Version, Change)] = {
    x.annotations.filter(_.tree.tpe =:= typeOf[Since]).map{ x1 =>
      x1.tree.children.tail match {
        case List(Literal(Constant(version: String)), Literal(Constant(change: String))) =>
          version -> change
        case _ => throw new Exception("bad args in N")
      }
    }.to(List)
  }
}