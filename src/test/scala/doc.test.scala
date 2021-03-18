package zero.protopurs
package doc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import zd.proto.api.N

class DocTest extends AnyFreeSpec with Matchers {
  val res = Purescript.generate[Push, Pull](moduleEncode="DocTest.Pull", moduleDecode="DocTest.Push", moduleCommon="DocTest.Common", category=_=>"All", ask="", ok="", err="")
  "log" in {
    res.doc._2 shouldBe List(
      "1.1.0" -> List("Translated" -> "rename 'values' to 'xs'")
    , "1.2.0" -> List("Translated" -> "change type of 'xs'")
    , "1.0.1" -> List("Ping" -> "added", "Translated" -> "added")
    )
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
@Since("1.0.1", "added")
@Since("1.1.0", "rename 'values' to 'xs'")
@Since("1.2.0", "change type of 'xs'")
@N(2) final case class Translated(@N(1) xs: Map[String, String]) extends Push
