package zd.proto.purs

import scala.reflect.runtime.universe._
import zd.gs.z._

object Doc {
  type Version = String
  type Change = String
  type ChangeLog = Seq[(Version, Seq[String])]
  def tex(xs: Seq[Tpe]): (String, ChangeLog) = {
    val changeLog = xs.flatMap{ x =>
      since(x.tpe.typeSymbol).map{ case (x1, y) => x1 -> s"${x.name}: $y" }
    }.groupBy(_._1).to(List).sortBy(_._1).reverse.map{ case (x, y) => x -> y.map(_._2).sorted}
    xs.flatMap{
      case x: TraitType =>
        val s = x.name + " := " + x.children.map(_.name).mkString(" | ")
        List(s).just
      case x @ (_: RegularType | _: RecursiveType | _: NoargsType) =>
        since(x.tpe.typeSymbol)
        (s"Fields for ${x.name}:" +:
          fields(x.tpe).map(y => y._1 + ": " + pursTypePars(y._2)._1)
        ).just
      case x: TupleType => Nothing
    }.flatten.mkString("\n") -> changeLog
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