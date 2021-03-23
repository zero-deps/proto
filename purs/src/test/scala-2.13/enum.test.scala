package zero.protopurs
package enum

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import zd.proto.api.N

class EnumSpec extends AnyFreeSpec with Matchers {
  val res = Purescript.generate[Push, Pull](moduleEncode="Enum.Pull", moduleDecode="Enum.Push", moduleCommon="Enum.Common", category=_=>"", ask="", ok="", err="")
  "enum" - {
    "print" in {
      res.purs.foreach{ case (filename, content) =>
        io.writeToFile(s"purs/test/src/$filename.purs", content)
      }
    }
  }
}

sealed trait Push
@N(1) case object Pong extends Push
@N(2) case class Saved(@N(1) x: String) extends Push

sealed trait Pull
@N(1) case object Ping extends Pull
@N(2) case class Save(@N(1) x: String) extends Pull
