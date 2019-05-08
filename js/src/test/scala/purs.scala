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
      res.decoders.toSet should be (Set(Snippets.decodePush, Snippets.decodeSiteOpt, Snippets.decodeSiteOpts))
    }
    "encoders" in {
      res.encoders.toSet should be (Set(Snippets.encodePull, Snippets.encodeGSP, Snippets.encodeGetSites, Snippets.encodeUploadChunk))
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
  val decodePush = """decodePush :: Uint8Array -> Effect (Maybe Push)
decodePush bytes = do
  let reader = createReader bytes
  tag <- uint32 reader
  case zshr tag 3 of
    1 -> do
      msglen <- uint32 reader
      x <- decodeSiteOpts reader msglen
      pure $ Just $ SiteOpts x
    _ ->
      pure Nothing"""

  val decodeSiteOpt = """decodeSiteOpt :: Reader -> Int -> Effect SiteOpt
decodeSiteOpt reader msglen = do
  let end = pos reader + msglen
  decode end { id: "", label: "" }
  where
    decode :: Int -> SiteOpt -> Effect SiteOpt
    decode end acc =
      if pos reader < end then do
        tag <- uint32 reader
        case zshr tag 3 of
          1 -> do
            x <- string reader
            decode end $ acc { id = x }
          2 -> do
            x <- string reader
            decode end $ acc { label = x }
          _ -> do
            skipType reader $ tag .&. 7
            decode end acc
      else pure acc"""

  val decodeSiteOpts = """decodeSiteOpts :: Reader -> Int -> Effect SiteOpts
decodeSiteOpts reader msglen = do
  let end = pos reader + msglen
  decode end { xs: [] }
  where
    decode :: Int -> SiteOpts -> Effect SiteOpts
    decode end acc =
      if pos reader < end then do
        tag <- uint32 reader
        case zshr tag 3 of
          1 -> do
            x <- uint32 reader >>= decodeSiteOpt reader
            decode end $ acc { xs = snoc acc.xs x }
          _ -> do
            skipType reader $ tag .&. 7
            decode end acc
      else pure acc"""

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

  val encodeGSP = """encodeGetSitePermissions :: Writer -> GetSitePermissions -> Effect Writer
encodeGetSitePermissions writer msg = do
  write_uint32 writer 10
  write_string writer msg.siteId
  pure writer"""

  val encodeGetSites = """encodeGetSites :: Writer -> GetSites -> Effect Writer
encodeGetSites writer _ = pure writer"""

  val encodeUploadChunk = """encodeUploadChunk :: Writer -> UploadChunk -> Effect Writer
encodeUploadChunk writer msg = do
  write_uint32 writer 10
  write_string writer msg.siteID
  sequence $ map (\x -> do
    write_uint32 writer 18
    write_string writer x
  ) msg.path
  write_uint32 writer 26
  write_string writer msg.name
  write_uint32 writer 34
  write_string writer msg.id
  write_uint32 writer 42
  write_bytes writer msg.chunk
  pure writer"""
}
