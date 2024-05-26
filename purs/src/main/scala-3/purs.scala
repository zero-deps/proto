package proto
package purs

import scala.quoted.*
import compiletime.asMatchable

trait Purs extends Ops:
  implicit val qctx: Quotes
  import qctx.reflect.*

  def nothingValue(name: String, tpe: TypeRepr): String = {
    if (tpe.isRepeated) {
      iterablePurs(tpe) match {
        case _: ArrayPurs => s"$name: []"
        case _: ArrayTuplePurs => s"$name: []"
      }
    } else s"$name: Nothing"
  }

  def justValue(name: String, tpe: TypeRepr): String = {
    if (tpe.isOption) name
    else if (tpe.isRepeated) name
    else {
      s"$name: Just $name"
    }
  }

  def makePursTypes(types: Seq[Tpe], genMaybe: Boolean): Seq[PursType] = {
    types.flatMap{
      case TraitType(tpe, name, children, _) =>
        List(PursType(List(
          s"data $name = ${children.map{
            case x if x.noargs => x.name
            case x if x.rec => s"${x.name}'' ${x.name}"
            case x => s"${x.name} ${x.name}"
          }.mkString(" | ")}"
        , s"derive instance eq$name :: Eq $name"
        ), `export`=Some(s"$name(..)")))
      case _: TupleType => Nil
      case _: NoargsType => Nil
      case RecursiveType(tpe, name) =>
        val f = fields(tpe)
        val fs = f.map{ case (name1, tpe, _, _) => name1 -> pursType(tpe) }
        val params = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._1}" }.mkString(", ")
        val x = s"newtype $name = $name { $params }"
        val eq = s"derive instance eq$name :: Eq $name"
        val defaults = f.collect{ case (name1, tpe1, _, v: HasDefFun) => (name1, pursType(tpe1)._1, v) }
        Seq(
          Some(PursType(List(x, eq), Some(s"$name($name)")))
        , if (defaults.nonEmpty) {
            val tmpl1 = s"default$name :: { ${defaults.map{ case (name1, tpe1, _) => s"$name1 :: $tpe1" }.mkString(", ")} }"
            val tmpl2 = s"default$name = { ${defaults.map{ case (name1, _, v) => s"$name1: ${v.value}" }.mkString(", ")} }"
            Some(PursType(Seq(tmpl1, tmpl2), Some(s"default$name")))
          } else None
        , if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._2}" }.mkString(", ")
            if (params != params1) {
              val x1 = s"type $name' = { $params1 }"
              Some(PursType(List(x1), None))
            } else None
          } else None
        ).flatten
      case RegularType(tpe, name) =>
        val f = fields(tpe)
        val fs = f.map{ case (name1, tpe1, _, _) => name1 -> pursType(tpe1) }
        val params = fs.map{ case (name1, tpe1) => s"$name1 :: ${tpe1._1}" }.mkString(", ")
        val x = if (params.nonEmpty) s"type $name = { $params }" else s"type $name = {}"
        val defaults = f.collect{ case (name1, tpe1, _, v: HasDefFun) => (name1, pursType(tpe1)._1, v) }
        Seq(
          Some(PursType(Seq(x), Some(s"$name")))
        , if (defaults.nonEmpty) {
            val tmpl1 = s"default$name :: { ${defaults.map{ case (name1, tpe1, _) => s"$name1 :: $tpe1" }.mkString(", ")} }"
            val tmpl2 = s"default$name = { ${defaults.map{ case (name1, _, v) => s"$name1: ${v.value}" }.mkString(", ")} }"
            Some(PursType(Seq(tmpl1, tmpl2), Some(s"default$name")))
          } else None
        , if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe1) => s"$name1 :: ${tpe1._2}" }.mkString(", ")
            if (params != params1) {
              val x1 = s"type $name' = { $params1 }"
              Some(PursType(Seq(x1), None))
            } else None
          } else None
        ).flatten
    }.distinct
  }