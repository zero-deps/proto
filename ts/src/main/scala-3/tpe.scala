package proto
package ts

final case class TsType(tmpl: Seq[String], `export`: Option[String])

final case class Coder(tmpl: String, `export`: Option[String])
