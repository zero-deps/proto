package proto
package ts

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import proto.encode
import proto.{caseCodecIdx, caseCodecAuto, sealedTraitCodecAuto}

class TypescriptSpec extends AnyFreeSpec with Matchers {
  "generate" - {
    "src" in {
      val res = proto.ts.generate[Push, Pull](moduleEncode="encode", moduleDecode="decode", moduleCommon="common", runtime="../proto")
      res.ts.foreach{ case (filename, content) =>
        io.writeToFile(s"ts/test/src/$filename.ts", content)
      }
    }
    "test" in {
      val res = proto.ts.generate[TestSchema, TestSchema](moduleEncode="schemaEncode", moduleDecode="schemaDecode", moduleCommon="schemaCommon", runtime="../proto")
      res.ts.foreach{ case (filename, content) =>
        io.writeToFile(s"ts/test/test/$filename.ts", content)
      }

      implicit val tc = caseCodecIdx[(String,String)]
      implicit val ac = caseCodecAuto[ClassWithMap]
      implicit val ac2 = caseCodecAuto[ClassWithLong]
      implicit val ac3 = caseCodecAuto[ClassWithInt]
      implicit val cwmc = sealedTraitCodecAuto[TestSchema]
      val hellomap = encode[TestSchema](new ClassWithMap(Map("en_GB"->"Hello", "it_IT"->"Ciao")))
      val Number_MAX_SAFE_INTEGER = 9007199254740991L
      val Number_MIN_SAFE_INTEGER = -9007199254740991L
      val maxlong = encode[TestSchema](new ClassWithLong(Number_MAX_SAFE_INTEGER))
      val minlong = encode[TestSchema](new ClassWithLong(Number_MIN_SAFE_INTEGER))
      val maxbigint = encode[TestSchema](new ClassWithLong(Long.MaxValue))
      val minbigint = encode[TestSchema](new ClassWithLong(Long.MinValue))
      val maxint = encode[TestSchema](new ClassWithInt(Int.MaxValue))
      val minint = encode[TestSchema](new ClassWithInt(Int.MinValue))
      def bytes_to_str(xs: Array[Byte]): String = xs.map(x => if x >= 0 then x.toString else (x+256).toString).mkString(",")
      val exp =
        s"""|import { TestSchema, ClassWithMap, ClassWithLong, ClassWithInt } from "./schemaCommon"
            |
            |export const map_schema: TestSchema = new ClassWithMap([ ["en_GB", "Hello"], ["it_IT", "Ciao"] ])
            |export const map_bytes: number[] = [${bytes_to_str(hellomap)}]
            |
            |export const maxlong_schema: TestSchema = new ClassWithLong(${Number_MAX_SAFE_INTEGER}n)
            |export const maxlong_bytes: number[] = [${bytes_to_str(maxlong)}]
            |
            |export const minlong_schema: TestSchema = new ClassWithLong(${Number_MIN_SAFE_INTEGER}n)
            |export const minlong_bytes: number[] = [${bytes_to_str(minlong)}]
            |
            |export const max_bigint_schema: TestSchema = new ClassWithLong(${Long.MaxValue}n)
            |export const max_bigint_bytes: number[] = [${bytes_to_str(maxbigint)}]
            |
            |export const min_bigint_schema: TestSchema = new ClassWithLong(${Long.MinValue}n)
            |export const min_bigint_bytes: number[] = [${bytes_to_str(minbigint)}]
            |
            |export const maxint_schema: TestSchema = new ClassWithInt(${Int.MaxValue})
            |export const maxint_bytes: number[] = [${bytes_to_str(maxint)}]
            |
            |export const minint_schema: TestSchema = new ClassWithInt(${Int.MinValue})
            |export const minint_bytes: number[] = [${bytes_to_str(minint)}]
            |""".stripMargin
      io.writeToFile("ts/test/test/cases.ts", exp)
    }
    "defaults" in {
      val res = proto.ts.generate[DPush, DPull](moduleEncode="defaultEncode", moduleDecode="defaultDecode", moduleCommon="defaultCommon", runtime="../proto")
      res.ts.foreach{ case (filename, content) =>
        io.writeToFile(s"ts/test/test/$filename.ts", content)
      }
    }
  }
}

sealed trait DPush
sealed trait DPull
@N(1) final case class SimpleT1(@N(1) m1: Option[Boolean], @N(2) b1: Boolean=false, @N(3) b2: String="") extends DPush with DPull
@N(2) final case class SimpleT2(@N(1) b0: Boolean, @N(2) b1: Boolean=false, @N(3) b2: String="") extends DPush with DPull

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
@N(5) case object Ping extends Push

@N(1300) final case class ComponentTemplateOk
  ( @N(1) fieldNode: FieldNode
  , @N(2) fieldNode1: FieldNode1
  ) extends Push

final case class SiteOpt(@N(1) id: String, @N(2) label: Option[String])
sealed trait PageType
@N(1) case object PageWidgets extends PageType
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
