package zd.proto.purs

import scala.reflect.runtime.universe._
import zd.gs.z._

object Doc {
  type Version = String
  type Change = String
  type ChangeLog = String
  def tex(xs: Seq[Tpe]): (String, ChangeLog) = {
    val changeLog = xs.flatMap{ x =>
      since(x.tpe.typeSymbol).map{ case (x1, y) => x1 -> s"  \\item{${x.name}} $y" }
    }.groupBy(_._1).to(List).sortBy(_._1).reverse.map{ case (x, y) => s"\\subsubsection{$x}" -> y.map(_._2).sorted.mkString("\n")}.map{
      case (x, "") => x
      case (x, y) => x + "\n\\begin{itemize}\n" + y + "\n\\end{itemize}"
    }.mkString("\n")
    xs.flatMap{
      case x: TraitType if x.firstLevel => Nothing
      case x: TraitType =>
        val a = x.children.map(y => s"  \\item ${y.name}").mkString("\n")
        val b = s"\\paragraph{${x.name}}\n" + (if (a.nonEmpty) s"\\begin{itemize}\n$a\n\\end{itemize}" else throw new Exception("no children: check @N on children"))
        b.just
      case x @ (_: RegularType | _: RecursiveType | _: NoargsType) =>
        val a = fields(x.tpe).map(y => s"  \\item[${y._1}] ${pursTypePars(y._2)._1}").mkString("\n")
        val b = if (a.nonEmpty) s"\\paragraph{${x.name}}\n\\begin{description}\n${a}\n\\end{description}" else s"\\paragraph{${x.name}} No fields"
        b.just
      case x: TupleType => Nothing
    }.mkString("\n") -> changeLog
  }
  
  private[this] def since(x: Symbol): Seq[(Version,Change)] = {
    x.annotations.filter(_.tree.tpe =:= typeOf[Since]).map{ x1 =>
      x1.tree.children.tail match {
        case List(Literal(Constant(version: String)), Literal(Constant(change: String))) =>
          version -> change
        case _ => throw new Exception("bad args in N")
      }
    }
  }
}