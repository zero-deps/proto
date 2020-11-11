package zero.protopurs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import zd.proto.api.{N, MessageCodec, encode}
import zd.proto.macrosapi.{caseCodecIdx, caseCodecAuto, sealedTraitCodecAuto}

class PurescriptSpec extends AnyFreeSpec with Matchers {
  "generate" - {
    "src" in {
      val tc: MessageCodec[(String,String)] = caseCodecIdx[(String,String)]
      val res = Purescript.generate[Push, Pull](moduleEncode="Pull", moduleDecode="Push", moduleCommon="Common", codecs=tc::Nil, category=_=>"", ask="", ok="", err="")
      res.purs.foreach{ case (filename, content) =>
        io.writeToFile(s"test/src/$filename.purs", content)
      }
    }
    "test" in {
      implicit val tc: MessageCodec[(String,String)] = caseCodecIdx[(String,String)]
      val res = Purescript.generate[TestSchema, TestSchema](moduleEncode="SchemaPull", moduleDecode="SchemaPush", moduleCommon="SchemaCommon", codecs=tc::Nil, category=_=>"", ask="", ok="", err="")
      res.purs.foreach{ case (filename, content) =>
        io.writeToFile(s"test/test/$filename.purs", content)
      }

      implicit val ac = caseCodecAuto[ClassWithMap]
      implicit val ac2 = caseCodecAuto[ClassWithLong]
      implicit val ac3 = caseCodecAuto[ClassWithInt]
      implicit val cwmc = sealedTraitCodecAuto[TestSchema]
      val hellomap = encode[TestSchema](new ClassWithMap(Map("en_GB"->"Hello", "it_IT"->"Ciao")))
      val Number_MAX_SAFE_INTEGER = 9007199254740991L
      val Number_MIN_SAFE_INTEGER = -9007199254740991L
      val maxlong = encode[TestSchema](new ClassWithLong(Number_MAX_SAFE_INTEGER))
      val minlong = encode[TestSchema](new ClassWithLong(Number_MIN_SAFE_INTEGER))
      val maxint = encode[TestSchema](new ClassWithInt(Int.MaxValue))
      val minint = encode[TestSchema](new ClassWithInt(Int.MinValue))
      def bytes_to_str(xs: Array[Byte]): String = xs.map(x => if (x >= 0) x.toString else (x+256).toString).mkString(" ")
      val exp =
        s"""|module Cases where
            |
            |import Prelude (negate)
            |import SchemaCommon
            |import Data.Tuple (Tuple(Tuple))
            |
            |map_schema :: TestSchema
            |map_schema = ClassWithMap { m: [ Tuple "en_GB" "Hello", Tuple "it_IT" "Ciao" ] }
            |
            |map_bytestr :: String
            |map_bytestr = "${bytes_to_str(hellomap)}"
            |
            |maxlong_schema :: TestSchema
            |maxlong_schema = ClassWithLong { x: $Number_MAX_SAFE_INTEGER.0 }
            |
            |maxlong_bytestr :: String
            |maxlong_bytestr = "${bytes_to_str(maxlong)}"
            |
            |minlong_schema :: TestSchema
            |minlong_schema = ClassWithLong { x: $Number_MIN_SAFE_INTEGER.0 }
            |
            |minlong_bytestr :: String
            |minlong_bytestr = "${bytes_to_str(minlong)}"
            |
            |maxint_schema :: TestSchema
            |maxint_schema = ClassWithInt { x: ${Int.MaxValue} }
            |
            |maxint_bytestr :: String
            |maxint_bytestr = "${bytes_to_str(maxint)}"
            |
            |minint_schema :: TestSchema
            |minint_schema = ClassWithInt { x: ${Int.MinValue} }
            |
            |minint_bytestr :: String
            |minint_bytestr = "${bytes_to_str(minint)}"
            |""".stripMargin
      io.writeToFile("test/test/Cases.purs", exp)
    }
  }
}

final case class FieldNode(@N(1) root: String, @N(2) forest: List[FieldNode])
final case class FieldNode1(@N(1) root: Option[String], @N(2) forest: List[FieldNode1])
sealed trait TestSchema
@N(1) final case class ClassWithMap(@N(1) m: Map[String,String]) extends TestSchema
@N(2) final case class ClassWithLong(@N(1) x: Long) extends TestSchema
@N(3) final case class ClassWithInt(@N(1) x: Int) extends TestSchema

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
