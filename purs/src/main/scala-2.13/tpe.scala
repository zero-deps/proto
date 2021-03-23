package proto
package purs

final case class PursType(tmpl: Seq[String], export: Option[String])

final case class Coder(tmpl: String, export: Option[String])
