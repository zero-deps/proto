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
      res.encodeData should be ("data Pull = GetSitePermissions GetSitePermissions | GetSites GetSites")
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
      ))
      ()
    }
    "decoders" in {
      res.decoders.toSet should be (Set(Snippets.decodePush, Snippets.decodeSiteOpt, Snippets.decodeSiteOpts))
    }
    "encoders" in {
      res.encoders.toSet should be (Set(Snippets.encodePull, Snippets.encodeGSP, Snippets.encodeGetSites))
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
    GetSitePermissions y -> do
      write_uint32 writer $ (shl 2 3) + 2
      writer_fork writer
      encodeGetSitePermissions writer y
      writer_ldelim writer
    GetSites y -> do
      write_uint32 writer $ (shl 1 3) + 2
      writer_fork writer
      encodeGetSites writer y
      writer_ldelim writer
  pure $ writer_finish writer"""

  val encodeGSP = """encodeGetSitePermissions :: Writer -> GetSitePermissions -> Effect Writer
encodeGetSitePermissions writer msg = do
  write_uint32 writer $ (shl 1 3) + 2
  write_string writer msg.siteId
  pure writer"""

  val encodeGetSites = """encodeGetSites :: Writer -> GetSites -> Effect Writer
encodeGetSites writer _ = pure writer"""
}
