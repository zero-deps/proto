package proto
package tex

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._

import Ops._

object Doc {
  def tex(messages: Seq[ChildMeta], others: Seq[Tpe], category: Int => String, ask: String, ok: String, err: String): String = {
    val messagestex = messages.groupBy(x => category(x.n)).toList.sortBy(_._1).map{
      case (cat, ys) =>
        s"""\\subsection{${cat}}
        |${correlation_tex(ys, ask, ok, err)}
        |${ys.map(y => type_to_tpe(y.tpe)._2).map(fields_tex).mkString("\n")}""".stripMargin
    }.mkString("\n")
    val otherstex = s"""\\newpage
      |\\subsection{Other Types}
      |${others.map(fields_tex).mkString("\n")}""".stripMargin
    (messagestex+"\n"+otherstex)
  }

  private def correlation_tex(xs: Seq[ChildMeta], ask: String, ok: String, err: String): String = {
    val xs1 = xs.map(_.name).groupBy(x => x.stripSuffix(ask).stripSuffix(ok).stripSuffix(err)).map{ case (prefix, names1) =>
      ( names1.filter(x => x.endsWith(ask))
      , names1.filter(_.endsWith(ok)) ++ names1.filter(_.endsWith(err))
      , names1.filterNot(x => x.endsWith(ask) || x.endsWith(ok) || x.endsWith(err))
      )
    }
    val xstex = Some(xs1.map{ case (a, b, c) =>
      val max = Math.max(Math.max(a.size, b.size), c.size)
      def f(xs: Seq[String], i: Int): String = {
        xs.lift(i).cata(x => s"\\hyperlink{$x}{$x}", "")
      }
      (0 until max).map(i =>
        s"""${f(a,i)} & ${f(b,i)} & ${f(c,i)}\\\\"""
      ).mkString("\n")
    }.mkString("\n")).filter(_.nonEmpty).cata("\\hline\n"+_+"\n\\hline", "\\hline\\hline")
    s"""\\begin{table}[H]
    |\\begin{tabular}{lll}
    |request & response & others\\\\
    |$xstex
    |\\end{tabular}
    |\\end{table}""".stripMargin
  }

  private val fields_tex: Tpe => String = {
    case x: TraitType if !x.firstLevel =>
      val n = 2
      val a = x.children.map(_.name).map(x => s"\\hyperlink{$x}{$x}").grouped(n).map(_.padTo(n,"").mkString("", " & ", "\\\\")).mkString(s"\\hline\n", "\n", "\n\\hline")
      val name = x.name
      if (a.nonEmpty) s"""\\begin{longtable}[l]{${"l".repeat(n)}}
        |\\multicolumn{$n}{l}{\\hypertarget{$name}{$name}}\\\\
        |$a
        |\\end{longtable}""".stripMargin
      else throw new Exception(s"no children: check @N on children for ${x.name}")
    case x @ (_: RegularType | _: RecursiveType | _: NoargsType) =>
      val fs = fields(x.tpe)
      val (rows, col3, align3) =
        if (hasdefval(fs)) (fs.map(y => s"${y._1} & ${pursTypeTex(y._2)} & ${defval(y._4)}\\\\"), " & default", "|r")
        else (fs.map(y => s"${y._1} & ${pursTypeTex(y._2)}\\\\"), "", "")
      import x.name
      s"""
      |\\begin{table}[H]
      |\\begin{tabular}{l|r$align3}
      |\\multicolumn{2}{l}{\\hypertarget{$name}{$name}}$col3\\\\
      |${Some(rows.mkString("\n")).filter(_.nonEmpty).cata("\\hline\n"+_+"\n\\hline", "\\hline\\hline")}
      |\\end{tabular}
      |\\end{table}
      |""".stripMargin.stripPrefix("\n").stripSuffix("\n")
    case _ => ""
  }

  private def hasdefval(xs: Seq[(Any,Any,Any,DefVal)]): Boolean = {
    xs.exists(x => defval(x._4).nonEmpty)
  }

  private val defval: DefVal => String = {
    case x: FillDef => x.value
    case _ => ""
  }
  
  private def pursTypeParsTex(tpe: Type): String = {
    if (tpe =:= StringClass.selfType) {
      "String"
    } else if (tpe =:= IntClass.selfType) {
      "Int"
    } else if (tpe =:= LongClass.selfType) {
      "Number"
    } else if (tpe =:= BooleanClass.selfType) {
      "Boolean"
    } else if (tpe =:= DoubleClass.selfType) {
      "Number"
    } else if (tpe =:= typeOf[Array[Byte]] || tpe =:= typeOf[Bytes]) {
      "Uint8Array"
    } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
      val typeArg = tpe.typeArgs.head
      if (typeArg =:= LongClass.selfType) {
        "(Maybe Number)"
      } else if (typeArg =:= DoubleClass.selfType) {
        "(Maybe Number)"
      } else {
        val name = typeArg.typeSymbol.name.encodedName.toString
        if (complexType(typeArg)) s"(Maybe \\hyperlink{$name}{$name})"
        else s"(Maybe $name)"
      }
    } else if (isIterable(tpe)) {
      iterablePurs(tpe) match {
        case ArrayPurs(tpe) =>
          val name = tpe.typeSymbol.asClass.name.encodedName.toString
          if (complexType(tpe)) s"[\\hyperlink{$name}{$name}]"
          else s"[$name]"
        case ArrayTuplePurs(tpe1, tpe2) =>
          val name1 = pursTypeParsTex(tpe1)
          val name2 = pursTypeParsTex(tpe2)
          s"[Tuple $name1 $name2]"
      }
    } else {
      val name = tpe.typeSymbol.name.encodedName.toString
      s"\\hyperlink{$name}{$name}"
    }
  }
  
  private def pursTypeTex(tpe: Type): String = {
    pursTypeParsTex(tpe).stripPrefix("(").stripSuffix(")")
  }

  def examples(messages: Seq[ChildMeta], commonTypes: Seq[Type]): String = {
    val content = 
      messages.map{ x =>
        typeArgsExample(x.tpe, commonTypes, ident=0) match {
          case ExampleRes(example,_,Nil) =>
            s"""${x.name}: '${example}'"""
          case ExampleRes(example,_,traits) =>
            s"""${x.name}: '${example}${newLine}${traitsAsComment(traits)}'"""
        }
      }.mkString(",\n  ")
    s"""
    |export const ApiExamples = {
    |  $content
    |}""".stripMargin.stripPrefix("\n")
  }

  private def traitsAsComment(traits: List[TraitVariants]): String = {
    traits
      .distinctBy(_.traitName)
      .map{ x => s"// ${x.traitName}:" :: x.variants.map("// " + _).appended("") }
      .flatten
      .mkString(newLine)
  }

  private val newLine = "\\n\\\n"

  private case class ExampleRes(
      example: String
    , comment: Option[String] = None
    , traits: List[TraitVariants] = Nil
  )

  private case class TraitVariants(
      traitName: String
    , variants: List[String]
  )

  private def typeArgsExample(
      tpe: Type
    , commonTypes: Seq[Type]
    , ident: Int
    , pretty: Boolean = true
    , recursiveTpe: Option[Type] = None
    , isTraitVariant: Boolean = false
  ): ExampleRes = {
    if (recursiveTpe.exists(_ =:= tpe)) {
      ExampleRes("", Some(tpe.typeSymbol.name.encodedName.toString))
    } else if (tpe =:= StringClass.selfType) {
      ExampleRes(""""string"""")
    } else if (tpe =:= IntClass.selfType) {
      ExampleRes("0")
    } else if (tpe =:= LongClass.selfType) {
      ExampleRes("0")
    } else if (tpe =:= BooleanClass.selfType) {
      ExampleRes("false")
    } else if (tpe =:= DoubleClass.selfType) {
      ExampleRes("0")
    } else if (tpe =:= typeOf[Array[Byte]] || tpe =:= typeOf[Bytes]) {
      ExampleRes("new Uint8Array([11, 22, 33])")
    } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
      val typeArg = tpe.typeArgs.head
      typeArgsExample(typeArg, commonTypes, ident, pretty, recursiveTpe) match {
        case e@ExampleRes(example,_,_) =>
          e.copy(example = s"new maybe.Just(${example})")
      }
    } else if (isIterable(tpe)) {
      iterablePurs(tpe) match {
        case ArrayPurs(tpe1) =>
          typeArgsExample(tpe1, commonTypes, ident, pretty, recursiveTpe) match {
            case e@ExampleRes(example,_,_) =>
              e.copy(example = s"[ $example ]")
          }
        case ArrayTuplePurs(tpe1, tpe2) =>
          val key = typeArgsExample(tpe1, commonTypes, ident, pretty, recursiveTpe)
          val value = typeArgsExample(tpe2, commonTypes, ident, pretty, recursiveTpe)
          value match {
            case e@ExampleRes(example,_,_) =>
              e.copy(example = s"[ new tuple.Tuple(${key.example}, $example) ]")
          }
      }
    } else if (isTrait(tpe)) {
      val traitName = tpe.typeSymbol.name.encodedName.toString
      val children = findChildren(tpe)
      val head = typeArgsExample(children.head.tpe, commonTypes, ident, pretty=pretty, recursiveTpe, isTraitVariant=true)
      val xs = children
        .map(x => typeArgsExample(x.tpe, commonTypes, ident, pretty=false, recursiveTpe, isTraitVariant=true))
      val allVariants = xs.map(_.example)
      val traits = xs.map(_.traits).flatten.toList
      head.copy(comment = Some(traitName), traits = TraitVariants(traitName, allVariants.toList) :: traits)
    } else {
      val recursiveTpe = if (isRecursive(tpe)) Some(tpe) else None
      val fs = fields(tpe)

      val (content, traits) =
        if (fs.isEmpty) "" -> Nil
        else {
          val (start, end, tabs, nextIdent) = 
            if (pretty) 
              (s"{$newLine", s"$newLine${"	"*ident}}", "	"*(ident+1), ident+1)
            else
              ("{ "        , " }"                     , ""           , ident  )

          val size = fs.size
          val res = fs.zipWithIndex.map{
            case ((fieldName,tpe,_,_), i) =>
              val isLast = (size - 1) == i
              typeArgsExample(tpe, commonTypes, nextIdent, pretty, recursiveTpe) match {
                case ExampleRes(example, Some(comment), traits) if pretty & isLast =>
                  s"""${tabs}${fieldName}: ${example} //${comment}""" -> traits
                case ExampleRes(example, Some(comment), traits) if pretty =>
                  s"""${tabs}${fieldName}: ${example}, //${comment}${newLine}""" -> traits
                case ExampleRes(example, None, traits) if pretty & isLast =>
                  s"""${tabs}${fieldName}: ${example}""" -> traits
                case ExampleRes(example, None, traits) if pretty =>
                  s"""${tabs}${fieldName}: ${example},${newLine}""" -> traits
                case ExampleRes(example, _, traits) if isLast =>
                  s"""${tabs}${fieldName}: ${example}""" -> traits
                case ExampleRes(example, _, traits) =>
                  s"""${tabs}${fieldName}: ${example}, """ -> traits
              }
          }
          res.map(_._1).mkString(start, "", end) -> res.map(_._2).flatten
        }
      
      if (isTraitVariant) {
        val name = tpe.typeSymbol.name.encodedName.toString
        if (commonTypes.exists(_ =:= tpe))
          ExampleRes(s"""new common.${name}(${content})""", None, traits)
        else
          ExampleRes(s"""new pull.${name}(${content})""", None, traits)
      } else
        ExampleRes(content, None, traits)
    }
  }
}