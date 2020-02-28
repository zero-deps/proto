package zd.proto.purs
package doc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import zd.proto.api.{N, MessageCodec}
import zd.proto.macrosapi.caseCodecIdx
// import zd.gs.z._

class DocTest extends AnyFreeSpec with Matchers {
  val tc: MessageCodec[(String,String)] = caseCodecIdx[(String,String)]
  val (_, doc) = Purescript.generate[Push, Pull](moduleEncode="DocTest.Pull", moduleDecode="DocTest.Push", moduleCommon="DocTest.Common", codecs=tc::Nil)
  "doc" - {
    "pull" in {
      doc.collectFirst{ case ("DocTest.Pull", x) => x }.get shouldBe """Pull := Ping | Ask
        |Fields for Ping:
        |Fields for Ask:
        |what: String""".stripMargin
    }
    "push" in {
      doc.collectFirst{ case ("DocTest.Push", x) => x }.get shouldBe """Push := Ping | Translated
        |Fields for Ping:
        |Fields for Translated:
        |xs: (Array (Tuple String String))""".stripMargin
    }
  }
}

sealed trait Push
sealed trait Pull
@N(1) final case object Ping extends Push with Pull
@N(2) final case class Ask(@N(1) what: String) extends Pull
@N(2) final case class Translated(@N(1) xs: Map[String, String]) extends Push
