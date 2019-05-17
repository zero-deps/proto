package zd
package proto

import org.scalatest.{FreeSpec, Matchers}
import zd.proto.api.N

class PurescriptSpec extends FreeSpec with Matchers {
  "purs has" - {
    val res = Purescript.generate[Push, Pull](moduleName="Api")
    "module name" in {
      res.prelude.startsWith("module Api")
    }
    "data type" in {
      res.decodeData should be ("data Push = SiteOpts SiteOpts")
      res.encodeData should be ("data Pull = GetSites GetSites | GetSitePermissions GetSitePermissions | UploadChunk UploadChunk")
      ()
    }
    "types" in {
      res.decodeTypes.toSet should be (Set(
        "type SiteOpt = { id :: String, label :: String }",
        "type SiteOpts = { xs :: Array SiteOpt }",
      ))
      res.encodeTypes.toSet should be (Set(
        "type GetSites = {  }",
        "type GetSitePermissions = { siteId :: String }",
        "type UploadChunk = { siteID :: String, path :: Array String, name :: String, id :: String, chunk :: Uint8Array }",
      ))
      ()
    }
    "decoders" in {
      res.decoders.foreach{
        case d if d.startsWith("decodePush :: ") => d should be (Snippets.decodePush)
        case d if d.startsWith("decodeSiteOpt :: ") => d should be (Snippets.decodeSiteOpt)
        case d if d.startsWith("decodeSiteOpts :: ") => d should be (Snippets.decodeSiteOpts)
      }
    }
    "encoders" in {
      res.encoders.foreach{
        case e if e.startsWith("encodePull :: ") => e should be (Snippets.encodePull)
        case e if e.startsWith("encodeGetSites :: ") => e should be (Snippets.encodeGetSites)
        case e if e.startsWith("encodeUploadChunk :: ") => e should be (Snippets.encodeUploadChunk)
        case e if e.startsWith("encodeGetSitePermissions :: ") => e should be (Snippets.encodeGSP)
      }
    }
    "print" in {
      println(res.format)
    }
  }
}

sealed trait Push
@N(1) final case class SiteOpts(@N(1) xs: Stream[SiteOpt]) extends Push

final case class SiteOpt(@N(1) id: String, @N(2) label: String)

sealed trait Pull
@N(1) final case class GetSites() extends Pull
@N(2) final case class GetSitePermissions(@N(1) siteId: String) extends Pull
@N(100) final case class UploadChunk
  ( @N(1) siteID: String
  , @N(3) name: String
  , @N(2) path: List[String]
  , @N(4) id: String
  , @N(5) chunk: Array[Byte]
  ) extends Pull

object Snippets {
  val decodePush = """decodePush :: Uint8Array -> Result Push
decodePush xs = do
  { offset: offset1, val: tag } <- read_uint32 xs 0
  case tag `zshr` 3 of
    1 -> do
      { offset: offset2, val: msglen } <- read_uint32 xs offset1
      { offset: offset3, val } <- decodeSiteOpts xs (offset1+offset2) msglen
      pure { offset: offset1+offset2+offset3, val: SiteOpts val }
    x ->
      Left $ BadType x"""

  val decodeSiteOpt = """decodeSiteOpt :: Uint8Array -> Int -> Int -> Result SiteOpt
decodeSiteOpt xs offset msglen = do
  let end = offset + msglen
  decode end { id: "", label: "" } offset
    where
    decode :: Int -> SiteOpt -> Int -> Result SiteOpt
    decode end acc offset1 =
      if offset1 < end then do
        { offset: offset2, val: tag } <- read_uint32 xs offset1
        case tag `zshr` 3 of
          1 -> do
            { offset: offset3, val } <- read_string xs $ offset1+offset2
            decode end (acc { id = val }) $ offset1+offset2+offset3
          2 -> do
            { offset: offset3, val } <- read_string xs $ offset1+offset2
            decode end (acc { label = val }) $ offset1+offset2+offset3
          _ -> do
            { offset: offset3 } <- skipType xs (offset1+offset2) $ tag .&. 7
            decode end acc $ offset1+offset2+offset3
      else pure { offset: offset1, val: acc }"""

  val decodeSiteOpts = """decodeSiteOpts :: Uint8Array -> Int -> Int -> Result SiteOpts
decodeSiteOpts xs offset msglen = do
  let end = offset + msglen
  decode end { xs: [] } offset
    where
    decode :: Int -> SiteOpts -> Int -> Result SiteOpts
    decode end acc offset1 =
      if offset1 < end then do
        { offset: offset2, val: tag } <- read_uint32 xs offset1
        case tag `zshr` 3 of
          1 -> do
            { offset: offset3, val: msglen1 } <- read_uint32 xs $ offset1+offset2
            { offset: offset4, val } <- decodeSiteOpt xs (offset1+offset2+offset3) msglen1
            decode end (acc { xs = snoc acc.xs val }) $ offset1+offset2+offset3+offset4
          _ -> do
            { offset: offset3 } <- skipType xs (offset1+offset2) $ tag .&. 7
            decode end acc $ offset1+offset2+offset3
      else pure { offset: offset1, val: acc }"""

  val encodePull = """encodePull :: Pull -> Effect Uint8Array
encodePull x = do
  let writer = createWriter unit
  case x of
    GetSites y -> do
      write_uint32 writer 10
      writer_fork writer
      encodeGetSites writer y
      writer_ldelim writer
    GetSitePermissions y -> do
      write_uint32 writer 18
      writer_fork writer
      encodeGetSitePermissions writer y
      writer_ldelim writer
    UploadChunk y -> do
      write_uint32 writer 802
      writer_fork writer
      encodeUploadChunk writer y
      writer_ldelim writer
  pure $ writer_finish writer"""

  val encodeGSP = """encodeGetSitePermissions :: Writer -> GetSitePermissions -> Effect Unit
encodeGetSitePermissions writer msg = do
  write_uint32 writer 10
  write_string writer msg.siteId
  pure unit"""

  val encodeGetSites = """encodeGetSites :: Writer -> GetSites -> Effect Unit
encodeGetSites writer _ = pure unit"""

  val encodeUploadChunk = """encodeUploadChunk :: Writer -> UploadChunk -> Effect Unit
encodeUploadChunk writer msg = do
  write_uint32 writer 10
  write_string writer msg.siteID
  void $ sequence $ map (\x -> do
    write_uint32 writer 18
    write_string writer x
  ) msg.path
  write_uint32 writer 26
  write_string writer msg.name
  write_uint32 writer 34
  write_string writer msg.id
  write_uint32 writer 42
  write_bytes writer msg.chunk
  pure unit"""
}
