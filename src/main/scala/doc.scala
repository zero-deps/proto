package zd.proto.purs

import zd.gs.z._

object Doc {
  def tex(xs: Seq[Tpe]): String = {
    xs.flatMap{
      case x: TraitType =>
        val s = x.name + " := " + x.children.map(_.name).mkString(" | ")
        List(s).just
      case x @ (_: RegularType | _: RecursiveType | _: NoargsType) =>
        (s"Fields for ${x.name}:" +:
          fields(x.tpe).map(y => y._1 + ": " + pursTypePars(y._2)._1)
        ).just
      case x: TupleType => Nothing
    }.flatten.mkString("\n")
  }
}