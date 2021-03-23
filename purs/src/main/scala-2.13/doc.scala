package purs

import scala.reflect.runtime.universe._
import zero.ext._, option._

object Doc {
  type Version = String
  type Change = String
  type Message = String
  type ChangeLog = List[(Version, Seq[(Message, Change)])]
  def tex(messages: Seq[ChildMeta], others: Seq[Tpe], all: Seq[Tpe], category: Int => String, ask: String, ok: String, err: String): (String, ChangeLog) = {
    val changeLog = all.flatMap{ x =>
      val message = x.name
      since(x.tpe.typeSymbol).map{ case (version, change) => version -> (message -> change) }
    }.groupBy(_._1).view.mapValues(_.map(_._2)).to(List)
    val messagestex = messages.groupBy(x => category(x.n)).toList.sortBy(_._1).map{
      case (cat, ys) =>
        s"""\\subsection{${cat}}
        |${correlation_tex(ys, ask, ok, err)}
        |${ys.map(y => type_to_tpe(y.tpe)._2).map(fields_tex).mkString("\n")}""".stripMargin
    }.mkString("\n")
    val otherstex = s"""\\newpage
      |\\subsection{Other Types}
      |${others.map(fields_tex).mkString("\n")}""".stripMargin
    (messagestex+"\n"+otherstex, changeLog)
  }

  private def correlation_tex(xs: Seq[ChildMeta], ask: String, ok: String, err: String): String = {
    val xs1 = xs.map(_.name).groupBy(x => x.stripSuffix(ask).stripSuffix(ok).stripSuffix(err)).map{ case (prefix, names1) =>
      ( names1.filter(x => x.endsWith(ask))
      , names1.filter(_.endsWith(ok)) ++ names1.filter(_.endsWith(err))
      , names1.filterNot(x => x.endsWith(ask) || x.endsWith(ok) || x.endsWith(err))
      )
    }
    val xstex = xs1.map{ case (a, b, c) =>
      val max = Math.max(Math.max(a.size, b.size), c.size)
      def f(xs: Seq[String], i: Int): String = {
        xs.lift(i).cata(x => s"\\hyperlink{$x}{$x}", "")
      }
      (0 until max).map(i =>
        s"""${f(a,i)} & ${f(b,i)} & ${f(c,i)}\\\\"""
      ).mkString("\n")
    }.mkString("\n").some.filter(_.nonEmpty).cata("\\hline\n"+_+"\n\\hline", "\\hline\\hline")
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
      |${rows.mkString("\n").some.filter(_.nonEmpty).cata("\\hline\n"+_+"\n\\hline", "\\hline\\hline")}
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
  
  private def since(x: Symbol): List[(Version, Change)] = {
    x.annotations.filter(_.tree.tpe =:= typeOf[Since]).map{ x1 =>
      x1.tree.children.tail match {
        case List(Literal(Constant(version: String)), Literal(Constant(change: String))) =>
          version -> change
        case _ => throw new Exception("bad args in N")
      }
    }.to(List)
  }
}
