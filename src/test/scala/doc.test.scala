package zero.protopurs
package doc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import zd.proto.api.{N, MessageCodec}
import zd.proto.macrosapi.caseCodecIdx

class DocTest extends AnyFreeSpec with Matchers {
  val tc: MessageCodec[(String,String)] = caseCodecIdx[(String,String)]
  val res = Purescript.generate[Push, Pull](moduleEncode="DocTest.Pull", moduleDecode="DocTest.Push", moduleCommon="DocTest.Common", codecs=tc::Nil)
  "doc" - {
    "pull" in {
      res.doc.collectFirst{ case ("DocTest.Pull", x) => x._1 }.get shouldBe
      """\paragraph{Ping} No fields
        |\paragraph{Ask}
        |\begin{description}
        |  \item[what] What
        |\end{description}
        |\paragraph{What}
        |\begin{itemize}
        |  \item Watt
        |  \item Who
        |\end{itemize}
        |\paragraph{Watt} No fields
        |\paragraph{Who}
        |\begin{description}
        |  \item[asks] String
        |\end{description}""".stripMargin
    }
    "push" in {
      res.doc.collectFirst{ case ("DocTest.Push", x) => x._1 }.get shouldBe
      """\paragraph{Ping} No fields
        |\paragraph{Translated}
        |\begin{description}
        |  \item[xs] (Array (Tuple String String))
        |\end{description}""".stripMargin
    }
  }
  "log" - {
    "pull" in {
      res.doc.collectFirst{ case ("DocTest.Pull", x) => x._2 }.get shouldBe
        """\subsubsection{1.0.1}
          |\begin{itemize}
          |  \item{Ping} added
          |\end{itemize}""".stripMargin
    }
    "push" in {
      res.doc.collectFirst{ case ("DocTest.Push", x) => x._2 }.get shouldBe
        """\subsubsection{1.2.0}
          |\begin{itemize}
          |  \item{Translated} change type of 'xs'
          |\end{itemize}
          |\subsubsection{1.1.0}
          |\begin{itemize}
          |  \item{Translated} rename 'values' to 'xs'
          |\end{itemize}
          |\subsubsection{1.0.1}
          |\begin{itemize}
          |  \item{Ping} added
          |  \item{Translated} added
          |\end{itemize}""".stripMargin
    }
  }
}

sealed trait Push
sealed trait Pull
@Since("1.0.1", "added")
@N(1) final case object Ping extends Push with Pull
@N(2) final case class Ask(@N(1) what: What) extends Pull
sealed trait What
@N(1) final case object Watt extends What
@N(2) final case class Who(@N(1) asks: String) extends What
@Since("1.2.0", "change type of 'xs'")
@Since("1.1.0", "rename 'values' to 'xs'")
@Since("1.0.1", "added")
@N(2) final case class Translated(@N(1) xs: Map[String, String]) extends Push
