package zd.proto.purs
package setmap

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import zd.proto.api.N
import zd.proto.macrosapi.{caseCodecIdx, caseCodecAuto, sealedTraitCodecAuto}
import scala.collection.immutable.ListSet

class SetMapSpec extends AnyFreeSpec with Matchers {
  val codecs = List({
    implicit val x = {
      implicit val y1 = caseCodecAuto[Dev.type]
      implicit val y2 = caseCodecAuto[Prod.type]
      sealedTraitCodecAuto[StepId]
    }
    caseCodecIdx[(StepId,ListSet[StepId])]
  }, {
    caseCodecIdx[(String,ListSet[String])]
  })
  val res = Purescript.generate[Push, Pull](moduleEncode="SetMap.Pull", moduleDecode="SetMap.Push", moduleCommon="SetMap.Common", codecs)
  "set/map" - {
    "print" in {
      res.foreach{ case (filename, content) =>
        io.writeToFile(s"test/src/$filename.purs", content)
      }
    }
  }
}

sealed trait Push
sealed trait Pull
@N(1) final case class Flow1(@N(1) graph: String Map ListSet[String]) extends Push with Pull
@N(2) final case class Flow2(@N(1) graph: StepId Map ListSet[StepId]) extends Push with Pull
sealed trait StepId
@N(1) final case object Prod extends StepId
@N(2) final case object Dev extends StepId
