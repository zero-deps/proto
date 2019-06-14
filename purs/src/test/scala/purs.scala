package zd
package proto

import org.scalatest.{FreeSpec, Matchers}
import zd.proto.api.N

class PurescriptSpec extends FreeSpec with Matchers {
  val res = Purescript.generate[Push, Pull](moduleName="Api")
  "purs has" - {
    "module name" in {
      res.prelude.startsWith("module Api")
    }
    "types" in {
      res.decodeTypes.toSet should be (Set(
        "data Push = Push_SiteOpts Push_SiteOpts | Push_Permissions Push_Permissions | Push_Page Push_Page",
        "data PageType = PageType_Widgets PageType_Widgets | PageType_Url PageType_Url",
        "type PageType_Url = { addr :: String }",
        "type PageType_Url' = { addr :: Maybe (String) }",
        "type PageType_Widgets = {  }",
        "type PageType_Widgets' = {  }",
        "type Push_Page = { tpe :: tpe }",
        "type Push_Page' = { tpe :: Maybe (tpe) }",
        "type Push_Permissions = { xs :: Array String }",
        "type Push_Permissions' = { xs :: Maybe (Array String) }",
        "type Push_SiteOpts = { xs :: Array SiteOpt }",
        "type Push_SiteOpts' = { xs :: Maybe (Array SiteOpt) }",
        "type SiteOpt = { id :: String, label :: String }",
        "type SiteOpt' = { id :: Maybe (String), label :: Maybe (String) }",
      ))
      res.encodeTypes.toSet should be (Set(
        "data Pull = Pull_GetSites Pull_GetSites | Pull_UploadChunk Pull_UploadChunk",
        "type Pull_GetSites = {  }",
        "type Pull_GetSites' = {  }",
        "type Pull_UploadChunk = { path :: Array String, id :: String, chunk :: Uint8Array }",
        "type Pull_UploadChunk' = { path :: Maybe (Array String), id :: Maybe (String), chunk :: Maybe (Uint8Array) }",
      ))
      ()
    }
    "decoders" in {
      res.decoders.foreach{
        case d if d.startsWith("decodePush :: ") => d should be (Snippets.decodePush)
        case d if d.startsWith("decodeSiteOpt :: ") => d should be (Snippets.decodeSiteOpt)
        case d if d.startsWith("decodeSiteOpts :: ") => d should be (Snippets.decodeSiteOpts)
        case d if d.startsWith("decodePermissions :: ") => d should be (Snippets.decodePermissions)
        case d if d.startsWith("decodePage :: ") => d should be (Snippets.decodePage)
        case d if d.startsWith("decodePageType :: ") => d should be (Snippets.decodePageType)
        case d if d.startsWith("decodePageType_Widgets :: ") => d should be (Snippets.decodePageType_Widgets)
        case d if d.startsWith("decodePageType_Url :: ") => d should be (Snippets.decodePageType_Url)
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
      println(Res.format(res))
    }
  }
}

sealed trait Push
@N(1) final case class SiteOpts(@N(1) xs: Stream[SiteOpt]) extends Push
@N(2) final case class Permissions(@N(1) xs: List[String]) extends Push
@N(3) final case class Page(@N(1) tpe: PageType) extends Push

final case class SiteOpt(@N(1) id: String, @N(2) label: String)
sealed trait PageType
@N(1) final case class Widgets() extends PageType
@N(2) final case class Url(@N(1) addr: String) extends PageType

sealed trait Pull
@N(1000) final case class GetSites() extends Pull
@N(1001) final case class UploadChunk
  ( @N(1) path: List[String]
  , @N(2) id: String
  , @N(3) chunk: Array[Byte]
  ) extends Pull

object Snippets {
  val decodePush = """decodePush :: Uint8Array -> Decode.Result Push
decodePush xs = do
  { pos: pos1, val: tag } <- Decode.uint32 xs 0
  case tag `zshr` 3 of
    1 -> do
      { pos: pos2, val } <- decodeSiteOpts xs pos1
      pure { pos: pos2, val: SiteOpts val }
    2 -> do
      { pos: pos2, val } <- decodePermissions xs pos1
      pure { pos: pos2, val: Permissions val }
    3 -> do
      { pos: pos2, val } <- decodePage xs pos1
      pure { pos: pos2, val: Page val }
    i ->
      Left $ Decode.BadType i"""

  val decodeSiteOpt = """decodeSiteOpt :: Uint8Array -> Int -> Decode.Result SiteOpt
decodeSiteOpt xs pos0 = do
  { pos, val: msglen } <- Decode.uint32 xs pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { id: Nothing, label: Nothing } pos
  case val of
    { id: Just id, label: Just label } -> pure { pos: pos1, val: { id, label } }
    _ -> Left $ Decode.Missing $ show val
    where
    decode :: Int -> SiteOpt' -> Int -> Decode.Result SiteOpt'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 xs pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string xs pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { id = Just val }) pos3
              2 ->
                case Decode.string xs pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { label = Just val }) pos3
              _ ->
                case Decode.skipType xs pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodeSiteOpts = """decodeSiteOpts :: Uint8Array -> Int -> Decode.Result SiteOpts
decodeSiteOpts xs pos0 = do
  { pos, val: msglen } <- Decode.uint32 xs pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { xs: [] } pos
  case val of
    { xs } -> pure { pos: pos1, val: { xs } }
    _ -> Left $ Decode.Missing $ show val
    where
    decode :: Int -> SiteOpts' -> Int -> Decode.Result SiteOpts'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 xs pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.uint32 xs pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val: msglen1 } ->
                    case decodeSiteOpt xs pos3 msglen1 of
                      Left x -> Left x
                      Right { pos: pos4, val } ->
                        decode end (acc { xs = snoc acc.xs val }) pos4
              _ ->
                case Decode.skipType xs pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePermissions = """decodePermissions :: Uint8Array -> Int -> Decode.Result Permissions
decodePermissions xs pos0 = do
  { pos, val: msglen } <- Decode.uint32 xs pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { xs: [] } pos
  case val of
    { xs } -> pure { pos: pos1, val: { xs } }
    _ -> Left $ Decode.Missing $ show val
    where
    decode :: Int -> Permissions' -> Int -> Decode.Result Permissions'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 xs pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string xs pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { xs = snoc acc.xs val }) pos3
              _ ->
                case Decode.skipType xs pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePage = """decodePage :: Uint8Array -> Int -> Decode.Result Page
decodePage xs pos0 = do
  { pos, val: msglen } <- Decode.uint32 xs pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { tpe: Nothing } pos
  case val of
    { tpe: Just tpe } -> pure { pos: pos1, val: { tpe } }
    _ -> Left $ Decode.Missing $ show val
    where
    decode :: Int -> Page' -> Int -> Decode.Result Page'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 xs pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodePageType xs pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { tpe = Just val }) pos3
              _ ->
                case Decode.skipType xs pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePageType = """decodePageType :: Uint8Array -> Int -> Decode.Result PageType
decodePageType xs pos0 = do
  { pos, val: msglen } <- Decode.uin32 xs pos0
  let end = pos + msglen
  decode end Nothing pos
    where
    decode :: Int -> Maybe PageType -> Int -> Decode.Result PageType
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 xs pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodePageType_Widgets xs pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (Just val) pos3
              2 ->
                case decodePageType_Url xs pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (Just val) pos3
              _ ->
                case Decode.skipType xs pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else
        case acc of
          Just x -> pure { pos: pos1, val: acc }
          Nothing -> Left $ Decode.Missing "PageType""""

  val decodePageType_Url = """decodePageType_Url :: Uint8Array -> Int -> Decode.Result PageType
decodePageType_Url xs pos = do
  { pos: pos1, val: msglen } <- Decode.uint32 xs pos
  let end = pos + msglen
  decode end { addr: "" } pos
    where
    decode :: Int -> PageType_Url -> Int -> Decode.Result PageType_Url
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uin32 xs pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string xs pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { addr = val }) pos3
              _ ->
                case Decode.skipType xs pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePageType_Widgets = """decodePageType_Widgets :: Uint8Array -> Int -> Decode.Result PageType
decodePageType_Widgets xs pos = do
  { pos: pos1, val: msglen } <- Decode.uint32 xs pos
  let end = pos + msglen
  decode end {  } pos
    where
    decode :: Int -> PageType_Widgets -> Int -> Decode.Result PageType_Widgets
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 xs pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              _ ->
                case Decode.skipType xs pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val encodePull = """encodePull :: Pull -> Uint8Array
encodePull (GetSites x) = concatAll [ Encode.uint32 8002, encodeGetSites x ]
encodePull (UploadChunk x) = concatAll [ Encode.uint32 8010, encodeUploadChunk x ]"""

  val encodeGetSites = """encodeGetSites :: GetSites -> Uint8Array
encodeGetSites _ = Encode.uint32 0"""

  val encodeUploadChunk = """encodeUploadChunk :: UploadChunk -> Uint8Array
encodeUploadChunk msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.uint32 10, Encode.string x ]) msg.path
        , Encode.uint32 18
        , Encode.string msg.id
        , Encode.uint32 26
        , Encode.bytes msg.chunk
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]"""
}
