package zd.proto.purs
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
      res.doc.collectFirst{ case ("DocTest.Pull", x) => x._1 }.get shouldBe """Pull := Ping | Ask
        |Fields for Ping:
        |Fields for Ask:
        |what: String""".stripMargin
    }
    "push" in {
      res.doc.collectFirst{ case ("DocTest.Push", x) => x._1 }.get shouldBe """Push := Ping | Translated
        |Fields for Ping:
        |Fields for Translated:
        |xs: (Array (Tuple String String))""".stripMargin
    }
  }
  "log" - {
    "pull" in {
      res.doc.collectFirst{ case ("DocTest.Pull", x) => x._2 }.get shouldBe Seq(("1.0.1", Seq("Ping: added")))
    }
    "push" in {
      res.doc.collectFirst{ case ("DocTest.Push", x) => x._2 }.get shouldBe Seq(("1.2.0", Seq("Translated: change type of 'xs'")), ("1.1.0", Seq("Translated: rename 'values' to 'xs'")), ("1.0.1", Seq("Ping: added", "Translated: added")))
    }
  }
}

sealed trait Push
sealed trait Pull
@Since("1.0.1", "added")
@N(1) final case object Ping extends Push with Pull
@N(2) final case class Ask(@N(1) what: String) extends Pull
@Since("1.2.0", "change type of 'xs'")
@Since("1.1.0", "rename 'values' to 'xs'")
@Since("1.0.1", "added")
@N(2) final case class Translated(@N(1) xs: Map[String, String]) extends Push
