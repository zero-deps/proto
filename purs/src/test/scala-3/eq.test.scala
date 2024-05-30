package proto
package purs
package eq

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EqSpec extends AnyFreeSpec with Matchers {
  val res = proto.purs.generate[Push, Pull](moduleEncode="EqSpec.Pull", moduleDecode="EqSpec.Push", moduleCommon="EqSpec.Common")
  "eq" - {
    "out" in {
      res.purs.foreach{ case (filename, content) =>
        io.writeToFile(s"purs/test/src/$filename.purs", content)
      }
    }
  }
}

sealed trait Push
sealed trait Pull
@N(1) case class Flow(@N(1) steps: Seq[FlowStep]) extends Push with Pull
sealed trait FlowStep
@N(1) case object Start extends FlowStep
@N(2) case class Ext(@N(1) tree: Node) extends FlowStep
case class Node(@N(1) root: String, @N(2) forest: Seq[Node])
@N(2) case class B(@N(1) a: A) extends Pull
sealed trait A
@N(1) case class C(@N(1) bytes: Array[Byte]) extends A
