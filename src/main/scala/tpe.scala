package zd.proto.purs

import scala.reflect.runtime.universe._
import zd.gs.z._

sealed trait Tpe { val tpe: Type }
final case class TraitType(tpe: Type, children: Seq[ChildMeta], firstLevel: Boolean) extends Tpe
final case class ChildMeta(name: String, tpe: Type, n: Int, noargs: Boolean)
final case class RegularType(tpe: Type) extends Tpe
final case class RecursiveType(tpe: Type) extends Tpe
final case class NoargsType(tpe: Type) extends Tpe
final case class TupleType(tpe: Type, tpe_1: Type, tpe_2: Type) extends Tpe

final case class PursType(tmpl: Seq[String], export: Maybe[String])