package proto

import scala.annotation.tailrec
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import zero.ext._, option._
import proto.Bytes

import Ops._

package object purs {
  def nothingValue(name: String, tpe: Type): String = {
    if (isIterable(tpe)) {
      iterablePurs(tpe) match {
        case _: ArrayPurs => s"$name: []"
        case _: ArrayTuplePurs => s"$name: []"
      }
    } else s"$name: Nothing"
  }
  
  def justValue(name: String, tpe: Type): String = {
    if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) name
    else if (isIterable(tpe)) name
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
        ), export=s"$name(..)".some))
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
          PursType(List(x, eq), s"$name($name)".some).some
        , if (defaults.nonEmpty) {
            val tmpl1 = s"default$name :: { ${defaults.map{ case (name1, tpe1, _) => s"$name1 :: $tpe1" }.mkString(", ")} }"
            val tmpl2 = s"default$name = { ${defaults.map{ case (name1, _, v) => s"$name1: ${v.value}" }.mkString(", ")} }"
            PursType(Seq(tmpl1, tmpl2), s"default$name".some).some
          } else None
        , if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._2}" }.mkString(", ")
            if (params != params1) {
              val x1 = s"type $name' = { $params1 }"
              PursType(List(x1), None).some
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
          PursType(Seq(x), s"$name".some).some
        , if (defaults.nonEmpty) {
            val tmpl1 = s"default$name :: { ${defaults.map{ case (name1, tpe1, _) => s"$name1 :: $tpe1" }.mkString(", ")} }"
            val tmpl2 = s"default$name = { ${defaults.map{ case (name1, _, v) => s"$name1: ${v.value}" }.mkString(", ")} }"
            PursType(Seq(tmpl1, tmpl2), s"default$name".some).some
          } else None
        , if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe1) => s"$name1 :: ${tpe1._2}" }.mkString(", ")
            if (params != params1) {
              val x1 = s"type $name' = { $params1 }"
              PursType(Seq(x1), None).some
            } else None
          } else None
        ).flatten
    }.distinct
  }
}
