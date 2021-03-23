package proto
package purs
package default

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import proto.N

class DefaultSpec extends AnyFreeSpec with Matchers {
  "generate" - {
    "src" in {
      val res = Purescript.generate[Push, Pull](moduleEncode="DefaultSpec.Pull", moduleDecode="DefaultSpec.Push", moduleCommon="DefaultSpec.Common")
      res.purs.foreach{ case (filename, content) =>
        io.writeToFile(s"purs/test/src/$filename.purs", content)
      }
    }
  }
}

sealed trait Push
sealed trait Pull
@N(1) final case class SimpleT1(@N(1) m1: Option[Boolean], @N(2) b1: Boolean=false, @N(3) b2: String="") extends Push with Pull
@N(2) final case class SimpleT2(@N(1) b0: Boolean, @N(2) b1: Boolean=false, @N(3) b2: String="") extends Push with Pull
@N(3) final case class RecursiveT1(@N(1) b1: Boolean=false, @N(2) b2: String="", @N(3) x: RecursiveT1) extends Push with Pull
@N(4) final case class RecursiveT2(@N(1) b1: Boolean=false, @N(2) b2: String="", @N(3) x: Option[RecursiveT2]) extends Push with Pull
@N(5) final case class OneMaybe(@N(1) m1: Option[String]) extends Push with Pull
@N(6) final case class OneSeq(@N(1) xs: Seq[String]) extends Push with Pull
