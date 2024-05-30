package proto
package purs
package `enum`

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EnumSpec extends AnyFreeSpec with Matchers {
  val res = proto.purs.generate[Push, Pull](moduleEncode="Enum.Pull", moduleDecode="Enum.Push", moduleCommon="Enum.Common")
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
