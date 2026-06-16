package proto
package ts

import scala.quoted.*

trait Types extends Ops:
  import qctx.reflect.*

  /** Interface field declaration, e.g. `descr: string` or `mobileSeo?: PageSeo`. */
  private def interfaceField(name: String, tpe: TypeRepr): String =
    if isOptionalField(tpe) then s"$name?: ${tsType(tpe)}"
    else s"$name: ${tsType(tpe)}"

  /** Class constructor parameter. Optional / defaulted params are emitted by the caller after
    * all required ones, so they may carry `?` or `= default`. */
  private def classParam(name: String, tpe: TypeRepr, defval: DefVal): String =
    defval match
      case NoneDef         => s"readonly $name?: ${tsType(tpe)}"
      case SeqDef          => s"readonly $name: ${tsType(tpe)} = []"
      case d: FillDef      => s"readonly $name: ${tsType(tpe)} = ${d.value}"
      case _               => s"readonly $name: ${tsType(tpe)}"

  private def isRequired(defval: DefVal): Boolean = defval match
    case NoDef => true
    case _     => false

  private def classDecl(name: String, fs: List[(String, TypeRepr, Int, DefVal)]): String =
    if fs.isEmpty then s"export class $name {}"
    else
      val (required, rest) = fs.partition{ case (_, _, _, dv) => isRequired(dv) }
      val params = (required ++ rest).map{ case (n, t, _, dv) => classParam(n, t, dv) }
      s"""|export class $name {
          |  constructor(
          |${params.map("    " + _ + ",").mkString("\n")}
          |  ) {}
          |}""".stripMargin

  private def interfaceDecl(name: String, fs: List[(String, TypeRepr, Int, DefVal)]): String =
    if fs.isEmpty then s"export interface $name {}"
    else
      val members = fs.map{ case (n, t, _, _) => interfaceField(n, t) }
      s"""|export interface $name {
          |${members.map("  " + _).mkString("\n")}
          |}""".stripMargin

  def makeTsTypes(types: Seq[Tpe], variants: Set[String]): Seq[TsType] =
    types.flatMap{
      case TraitType(_, name, children, _) =>
        val union = children.map(_.name).mkString(" | ")
        List(TsType(List(s"export type $name = $union"), Some(name)))
      case _: TupleType => Nil
      case NoargsType(_, name) =>
        if variants.contains(name) then List(TsType(List(s"export class $name {}"), Some(name)))
        else List(TsType(List(s"export interface $name {}"), Some(name)))
      case RegularType(tpe, name) =>
        val fs = fields(tpe)
        val decl = if variants.contains(name) then classDecl(name, fs) else interfaceDecl(name, fs)
        List(TsType(List(decl), Some(name)))
      case RecursiveType(tpe, name) =>
        val fs = fields(tpe)
        val decl = if variants.contains(name) then classDecl(name, fs) else interfaceDecl(name, fs)
        List(TsType(List(decl), Some(name)))
    }.distinct
