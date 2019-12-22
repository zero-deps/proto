package zd.proto.purs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import zd.proto.api.{N, MessageCodec, encode}
import zd.proto.macrosapi.{caseCodecIdx, caseCodecAuto, sealedTraitCodecAuto}

class PurescriptSpec extends AnyFreeSpec with Matchers {
  val tc: MessageCodec[(String,String)] = caseCodecIdx[(String,String)]
  val res2 = Purescript.generate[Push, Pull](moduleEncodeName="Pull", moduleDecodeName="Push", "Common", codecs=tc::Nil)
  "purs has" - {
    "module name" in {
      res2.encode.prelude.startsWith("module Pull")
      res2.decode.prelude.startsWith("module Push")
      res2.common.prelude.startsWith("module Common")
    }
    "print" in {
      // println(Res.format(res2.common))
      Res.writeToFile("test/src/Common.purs", res2.common)
      // println(Res.format(res2.decode))
      Res.writeToFile("test/src/Push.purs", res2.decode)
      // println(Res.format(res2.encode))
      Res.writeToFile("test/src/Pull.purs", res2.encode)
    }
    "purs tests" in {
      implicit val tc: MessageCodec[(String,String)] = caseCodecIdx[(String,String)]
      val testres = Purescript.generate[TestSchema, TestSchema](moduleEncodeName="SchemaPull", moduleDecodeName="SchemaPush", "SchemaCommon", codecs=tc::Nil)
      Res.writeToFile("test/test/SchemaCommon.purs", testres.common)
      Res.writeToFile("test/test/SchemaPull.purs", testres.encode)
      Res.writeToFile("test/test/SchemaPush.purs", testres.decode)

      implicit val ac: MessageCodec[ClassWithMap] = caseCodecAuto[ClassWithMap]
      implicit val cwmc: MessageCodec[TestSchema] = sealedTraitCodecAuto[TestSchema]
      val r1 = encode[TestSchema](new ClassWithMap(Map("en_GB"->"Name", "ro_RO"->"Nome")))
      val exp =
        s"""|module Cases where
            |
            |import SchemaCommon
            |import Data.Tuple (Tuple(Tuple))
            |
            |c1 :: TestSchema
            |c1 = ClassWithMap { m: [ Tuple "en_GB" "Name", Tuple "ro_RO" "Nome" ] }
            |
            |r1 :: String
            |r1 = "${r1.mkString(" ")}"""".stripMargin
      Res.writeToFile("test/test/Cases.purs", exp)
    }
  }
}

final case class FieldNode(@N(1) root: String, @N(2) forest: List[FieldNode])
final case class FieldNode1(@N(1) root: Option[String], @N(2) forest: List[FieldNode1])
sealed trait TestSchema
@N(1) final case class ClassWithMap(@N(1) m: Map[String,String]) extends TestSchema

sealed trait Push
@N(1) final case class SiteOpts(@N(1) xs: LazyList[SiteOpt]) extends Push
@N(2) final case class Permissions(@N(1) xs: List[String]) extends Push
@N(3) final case class Page(@N(1) tpe: PageType, @N(2) guest: Boolean, @N(3) seo: PageSeo, @N(4) mobileSeo: Option[PageSeo], @N(5) name: Map[String,String]) extends Push
final case class PageSeo(@N(1) descr: String, @N(2) order: Double)
@N(4) final case class PageTreeItem(@N(1) priority: Int) extends Push
@N(5) final case object Ping extends Push

@N(1300) final case class ComponentTemplateOk
  ( @N(1) fieldNode: FieldNode
  , @N(2) fieldNode1: FieldNode1
  ) extends Push

final case class SiteOpt(@N(1) id: String, @N(2) label: Option[String])
sealed trait PageType
@N(1) final case object PageWidgets extends PageType
@N(2) final case class PageUrl(@N(1) addr: String) extends PageType

sealed trait Pull
@N(1000) final case class GetSites() extends Pull
@N(1001) final case class UploadChunk
  ( @N(1) path: List[String]
  , @N(2) id: String
  , @N(3) chunk: Array[Byte]
  ) extends Pull
@N(1002) final case class SavePage(@N(1) tpe: PageType, @N(2) guest: Boolean, @N(3) seo: PageSeo, @N(4) mobileSeo: Option[PageSeo], @N(5) name: Map[String,String]) extends Pull
@N(1400) final case class SaveComponentTemplate
  ( @N(1) fieldNode: FieldNode
  ) extends Pull
@N(1920) final case class ComponentsSavePrefs
  ( @N(1) id: String
  , @N(2) pageid: String
  , @N(3) siteid: String
  , @N(4) tree: FieldNode
  , @N(5) extTree: Option[FieldNode]
  ) extends Pull
