package zd.proto.purs
package eq

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import zd.proto.api.N

class EqSpec extends AnyFreeSpec with Matchers {
  val res = Purescript.generate[Push, Pull](moduleEncode="EqSpec.Pull", moduleDecode="EqSpec.Push", moduleCommon="EqSpec.Common", codecs=Nil)
  "eq" - {
    "out" in {
      res.foreach{ case (filename, content) =>
        io.writeToFile(s"test/src/$filename.purs", content)
      }
    }
  }
}

sealed trait Push
sealed trait Pull
@N(1) final case class Flow(@N(1) steps: Seq[FlowStep]) extends Push with Pull
sealed trait FlowStep
@N(1) final case object Start extends FlowStep
@N(2) final case class Ext(@N(1) tree: Node) extends FlowStep
final case class Node(@N(1) root: String, @N(2) forest: Seq[Node])