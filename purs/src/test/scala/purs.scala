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
      res.decodeData should be ("data Push = SiteOpts SiteOpts | Permissions Permissions")
      res.encodeData should be ("data Pull = GetSites GetSites | UploadChunk UploadChunk")
      ()
    }
    "types" in {
      res.decodeTypes.toSet should be (Set(
        "type SiteOpt = { id :: String, label :: String }",
        "type SiteOpts = { xs :: Array SiteOpt }",
        "type Permissions = { xs :: Array String }",
      ))
      res.encodeTypes.toSet should be (Set(
        "type GetSites = {  }",
        "type UploadChunk = { siteID :: String, path :: Array String, name :: String, id :: String, chunk :: Uint8Array }",
      ))
      ()
    }
    "decoders" in {
      res.decoders.foreach{
        case d if d.startsWith("decodePush :: ") => d should be (Snippets.decodePush)
        case d if d.startsWith("decodeSiteOpt :: ") => d should be (Snippets.decodeSiteOpt)
        case d if d.startsWith("decodeSiteOpts :: ") => d should be (Snippets.decodeSiteOpts)
        case d if d.startsWith("decodePermissions :: ") => d should be (Snippets.decodePermissions)
      }
    }
    "encoders" in {
      res.encoders.foreach{
        case e if e.startsWith("encodePull :: ") => e should be (Snippets.encodePull)
        case e if e.startsWith("encodeGetSites :: ") => e should be (Snippets.encodeGetSites)
        case e if e.startsWith("encodeUploadChunk :: ") => e should be (Snippets.encodeUploadChunk)
      }
    }
    "print" in {
      println(res.format)
    }
  }
}

sealed trait Push
@N(1) final case class SiteOpts(@N(1) xs: Stream[SiteOpt]) extends Push
@N(2) final case class Permissions(@N(1) xs: List[String]) extends Push

final case class SiteOpt(@N(1) id: String, @N(2) label: String)

sealed trait Pull
@N(1) final case class GetSites() extends Pull
@N(100) final case class UploadChunk
  ( @N(1) siteID: String
  , @N(3) name: String
  , @N(2) path: List[String]
  , @N(4) id: String
  , @N(5) chunk: Array[Byte]
  ) extends Pull

object Snippets {
  val decodePush = """decodePush :: Uint8Array -> Decode.Result Push
decodePush xs = do
  { pos: pos1, val: tag } <- Decode.uint32 xs 0
  case tag `zshr` 3 of
    1 -> do
      { pos: pos2, val: msglen } <- Decode.uint32 xs pos1
      { pos: pos3, val } <- decodeSiteOpts xs pos2 msglen
      pure { pos: pos3, val: SiteOpts val }
    2 -> do
      { pos: pos2, val: msglen } <- Decode.uint32 xs pos1
      { pos: pos3, val } <- decodePermissions xs pos2 msglen
      pure { pos: pos3, val: Permissions val }
    i ->
      Left $ Decode.BadType i"""

  val decodeSiteOpt = """decodeSiteOpt :: Uint8Array -> Int -> Int -> Decode.Result SiteOpt
decodeSiteOpt xs pos msglen = do
  let end = pos + msglen
  decode end { id: "", label: "" } pos
    where
    decode :: Int -> SiteOpt -> Int -> Decode.Result SiteOpt
    decode end acc pos1 =
      if pos1 < end then do
        { pos: pos2, val: tag } <- Decode.uint32 xs pos1
        case tag `zshr` 3 of
          1 -> do
            { pos: pos3, val } <- Decode.string xs pos2
            decode end (acc { id = val }) pos3
          2 -> do
            { pos: pos3, val } <- Decode.string xs pos2
            decode end (acc { label = val }) pos3
          _ -> do
            { pos: pos3 } <- Decode.skipType xs pos2 $ tag .&. 7
            decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodeSiteOpts = """decodeSiteOpts :: Uint8Array -> Int -> Int -> Decode.Result SiteOpts
decodeSiteOpts xs pos msglen = do
  let end = pos + msglen
  decode end { xs: [] } pos
    where
    decode :: Int -> SiteOpts -> Int -> Decode.Result SiteOpts
    decode end acc pos1 =
      if pos1 < end then do
        { pos: pos2, val: tag } <- Decode.uint32 xs pos1
        case tag `zshr` 3 of
          1 -> do
            { pos: pos3, val: msglen1 } <- Decode.uint32 xs pos2
            { pos: pos4, val } <- decodeSiteOpt xs pos3 msglen1
            decode end (acc { xs = snoc acc.xs val }) pos4
          _ -> do
            { pos: pos3 } <- Decode.skipType xs pos2 $ tag .&. 7
            decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePermissions = """decodePermissions :: Uint8Array -> Int -> Int -> Decode.Result Permissions
decodePermissions xs pos msglen = do
  let end = pos + msglen
  decode end { xs: [] } pos
    where
    decode :: Int -> Permissions -> Int -> Decode.Result Permissions
    decode end acc pos1 =
      if pos1 < end then do
        { pos: pos2, val: tag } <- Decode.uint32 xs pos1
        case tag `zshr` 3 of
          1 -> do
            { pos: pos3, val } <- Decode.string xs pos2
            decode end (acc { xs = snoc acc.xs val }) pos3
          _ -> do
            { pos: pos3 } <- Decode.skipType xs pos2 $ tag .&. 7
            decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val encodePull = """encodePull :: Pull -> Uint8Array
encodePull (GetSites x) = concatAll [ Encode.uint32 10, encodeGetSites x ]
encodePull (UploadChunk x) = concatAll [ Encode.uint32 802, encodeUploadChunk x ]"""

  val encodeGetSites = """encodeGetSites :: GetSites -> Uint8Array
encodeGetSites _ = Encode.uint32 0"""

  val encodeUploadChunk = """encodeUploadChunk :: UploadChunk -> Uint8Array
encodeUploadChunk msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.siteID
        , concatAll $ concatMap (\x -> [ Encode.uint32 18, Encode.string x ]) msg.path
        , Encode.uint32 26
        , Encode.string msg.name
        , Encode.uint32 34
        , Encode.string msg.id
        , Encode.uint32 42
        , Encode.bytes msg.chunk
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]"""
}
