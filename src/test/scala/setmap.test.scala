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
  val res2 = Purescript.generate[Push, Pull](moduleEncodeName="SetMap.Pull", moduleDecodeName="SetMap.Push", "SetMap.Common", codecs)
  "set/map" - {
    "print" in {
      // println(Res.format(res2.common))
      Res.writeToFile("test/src/SetMap/Common.purs", res2.common)
      // println(Res.format(res2.decode))
      Res.writeToFile("test/src/SetMap/Push.purs", res2.decode)
      // println(Res.format(res2.encode))
      Res.writeToFile("test/src/SetMap/Pull.purs", res2.encode)
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
