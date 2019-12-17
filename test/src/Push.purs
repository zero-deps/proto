module Push where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit, unit)
import Prelude (bind, pure, ($), (+), (<))
import Proto.Decode as Decode
import Common

data Push = SiteOpts SiteOpts | Permissions Permissions | Page Page | PageTreeItem PageTreeItem | ComponentTemplateOk ComponentTemplateOk
type SiteOpts = { xs :: Array SiteOpt }
type SiteOpts' = { xs :: Array SiteOpt }
type SiteOpt = { id :: String, label :: Maybe String }
type SiteOpt' = { id :: Maybe String, label :: Maybe String }
type Permissions = { xs :: Array String }
type Permissions' = { xs :: Array String }
type Page = { tpe :: PageType, guest :: Boolean, seo :: PageSeo, mobileSeo :: Maybe PageSeo, name :: Array (Tuple String String) }
type Page' = { tpe :: Maybe PageType, guest :: Maybe Boolean, seo :: Maybe PageSeo, mobileSeo :: Maybe PageSeo, name :: Array (Tuple String String) }
type PageUrl' = { addr :: Maybe String }
type PageSeo' = { descr :: Maybe String, order :: Maybe Number }
type PageTreeItem = { priority :: Int }
type PageTreeItem' = { priority :: Maybe Int }
type ComponentTemplateOk = { fieldNode :: FieldNode }
type ComponentTemplateOk' = { fieldNode :: Maybe FieldNode }
newtype FieldNode' = FieldNode' { root :: Maybe String, forest :: Array FieldNode }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> do
      { pos: pos2, val } <- decodeSiteOpts _xs_ pos1
      pure { pos: pos2, val: SiteOpts val }
    2 -> do
      { pos: pos2, val } <- decodePermissions _xs_ pos1
      pure { pos: pos2, val: Permissions val }
    3 -> do
      { pos: pos2, val } <- decodePage _xs_ pos1
      pure { pos: pos2, val: Page val }
    4 -> do
      { pos: pos2, val } <- decodePageTreeItem _xs_ pos1
      pure { pos: pos2, val: PageTreeItem val }
    1300 -> do
      { pos: pos2, val } <- decodeComponentTemplateOk _xs_ pos1
      pure { pos: pos2, val: ComponentTemplateOk val }
    i ->
      Left $ Decode.BadType i

decodeSiteOpts :: Uint8Array -> Int -> Decode.Result SiteOpts
decodeSiteOpts _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { xs: [] } pos
  case val of
    { xs } -> pure { pos: pos1, val: { xs } }
    where
    decode :: Int -> SiteOpts' -> Int -> Decode.Result SiteOpts'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodeSiteOpt _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { xs = snoc acc.xs val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodeSiteOpt :: Uint8Array -> Int -> Decode.Result SiteOpt
decodeSiteOpt _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { id: Nothing, label: Nothing } pos
  case val of
    { id: Just id, label } -> pure { pos: pos1, val: { id, label } }
    _ -> Left $ Decode.MissingFields "SiteOpt"
    where
    decode :: Int -> SiteOpt' -> Int -> Decode.Result SiteOpt'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { id = Just val }) pos3
              2 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { label = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodePermissions :: Uint8Array -> Int -> Decode.Result Permissions
decodePermissions _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { xs: [] } pos
  case val of
    { xs } -> pure { pos: pos1, val: { xs } }
    where
    decode :: Int -> Permissions' -> Int -> Decode.Result Permissions'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { xs = snoc acc.xs val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodePage :: Uint8Array -> Int -> Decode.Result Page
decodePage _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { tpe: Nothing, guest: Nothing, seo: Nothing, mobileSeo: Nothing, name: [] } pos
  case val of
    { tpe: Just tpe, guest: Just guest, seo: Just seo, mobileSeo, name } -> pure { pos: pos1, val: { tpe, guest, seo, mobileSeo, name } }
    _ -> Left $ Decode.MissingFields "Page"
    where
    decode :: Int -> Page' -> Int -> Decode.Result Page'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodePageType _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { tpe = Just val }) pos3
              2 ->
                case Decode.boolean _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { guest = Just val }) pos3
              3 ->
                case decodePageSeo _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { seo = Just val }) pos3
              4 ->
                case decodePageSeo _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { mobileSeo = Just val }) pos3
              5 ->
                case decodeStringString _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { name = snoc acc.name val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodePageType :: Uint8Array -> Int -> Decode.Result PageType
decodePageType _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  decode end Nothing pos
    where
    decode :: Int -> Maybe PageType -> Int -> Decode.Result PageType
    decode end acc pos1 | pos1 < end =
      case Decode.uint32 _xs_ pos1 of
        Left x -> Left x
        Right { pos: pos2, val: tag } ->
          case tag `zshr` 3 of
            1 ->
              case decodePageWidgets _xs_ pos2 of
                Left x -> Left x
                Right { pos: pos3 } -> decode end (Just PageWidgets) pos3
            2 ->
              case decodePageUrl _xs_ pos2 of
                Left x -> Left x
                Right { pos: pos3, val } ->
                  decode end (Just $ PageUrl val) pos3
            _ ->
              case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                Left x -> Left x
                Right { pos: pos3 } ->
                  decode end acc pos3
    decode end (Just acc) pos1 = pure { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "PageType"

decodePageWidgets :: Uint8Array -> Int -> Decode.Result Unit
decodePageWidgets _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  pure { pos: end, val: unit }

decodePageUrl :: Uint8Array -> Int -> Decode.Result PageUrl
decodePageUrl _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { addr: Nothing } pos
  case val of
    { addr: Just addr } -> pure { pos: pos1, val: { addr } }
    _ -> Left $ Decode.MissingFields "PageUrl"
    where
    decode :: Int -> PageUrl' -> Int -> Decode.Result PageUrl'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { addr = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodePageSeo :: Uint8Array -> Int -> Decode.Result PageSeo
decodePageSeo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { descr: Nothing, order: Nothing } pos
  case val of
    { descr: Just descr, order: Just order } -> pure { pos: pos1, val: { descr, order } }
    _ -> Left $ Decode.MissingFields "PageSeo"
    where
    decode :: Int -> PageSeo' -> Int -> Decode.Result PageSeo'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { descr = Just val }) pos3
              2 ->
                case Decode.double _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { order = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodeStringString :: Uint8Array -> Int -> Decode.Result (Tuple String String)
decodeStringString _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { first: Nothing, second: Nothing } pos
  case val of
    { first: Just first, second: Just second } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $ Decode.MissingFields "decodeStringString"
    where
    decode :: Int -> { first :: Maybe String, second :: Maybe String } -> Int -> Decode.Result { first :: Maybe String, second :: Maybe String }
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { first = Just val }) pos3
              2 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { second = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodePageTreeItem :: Uint8Array -> Int -> Decode.Result PageTreeItem
decodePageTreeItem _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { priority: Nothing } pos
  case val of
    { priority: Just priority } -> pure { pos: pos1, val: { priority } }
    _ -> Left $ Decode.MissingFields "PageTreeItem"
    where
    decode :: Int -> PageTreeItem' -> Int -> Decode.Result PageTreeItem'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.int32 _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { priority = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodeComponentTemplateOk :: Uint8Array -> Int -> Decode.Result ComponentTemplateOk
decodeComponentTemplateOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { fieldNode: Nothing } pos
  case val of
    { fieldNode: Just fieldNode } -> pure { pos: pos1, val: { fieldNode } }
    _ -> Left $ Decode.MissingFields "ComponentTemplateOk"
    where
    decode :: Int -> ComponentTemplateOk' -> Int -> Decode.Result ComponentTemplateOk'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodeFieldNode _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { fieldNode = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodeFieldNode :: Uint8Array -> Int -> Decode.Result FieldNode
decodeFieldNode _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val: FieldNode' val } <- decode end (FieldNode' { root: Nothing, forest: [] }) pos
  case val of
    { root: Just root, forest } -> pure { pos: pos1, val: FieldNode { root, forest } }
    _ -> Left $ Decode.MissingFields "FieldNode"
    where
    decode :: Int -> FieldNode' -> Int -> Decode.Result FieldNode'
    decode end (FieldNode' acc) pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (FieldNode' $ acc { root = Just val }) pos3
              2 ->
                case decodeFieldNode _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (FieldNode' $ acc { forest = snoc acc.forest val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end (FieldNode' acc) pos3
      else pure { pos: pos1, val: FieldNode' acc }