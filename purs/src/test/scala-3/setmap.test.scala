package proto
package purs
package setmap

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable.ListSet

class SetMapSpec extends AnyFreeSpec with Matchers {
  val res = proto.purs.generate[Push, Pull](moduleEncode="SetMap.Pull", moduleDecode="SetMap.Push", moduleCommon="SetMap.Common")
  "set/map" - {
    "print" in {
      res.purs.foreach{ case (filename, content) =>
        io.writeToFile(s"purs/test/src/$filename.purs", content)
      }
    }
  }
}

sealed trait Push
sealed trait Pull
@N(1) final case class Flow1(@N(1) graph: String Map ListSet[String]) extends Push with Pull
@N(2) final case class Flow2(@N(1) graph: StepId Map ListSet[StepId]) extends Push with Pull
sealed trait StepId
@N(1) case object Prod extends StepId
@N(2) case object Dev extends StepId
