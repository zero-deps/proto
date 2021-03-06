module Push
  ( Push(..)
  , SiteOpts
  , defaultSiteOpts
  , SiteOpt
  , defaultSiteOpt
  , Permissions
  , defaultPermissions
  , Page
  , defaultPage
  , PageTreeItem
  , ComponentTemplateOk
  , FieldNode1(FieldNode1)
  , defaultFieldNode1
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Array (snoc)
import Data.Either (Either(Left))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit, unit)
import Prelude (map, bind, pure, ($), (+), (<), (<<<))
import Proto.Decode as Decode
import Proto.Uint8Array (Uint8Array)
import Common

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = SiteOpts SiteOpts | Permissions Permissions | Page Page | PageTreeItem PageTreeItem | Ping | ComponentTemplateOk ComponentTemplateOk
derive instance eqPush :: Eq Push
type SiteOpts = { xs :: Array SiteOpt }
defaultSiteOpts :: { xs :: Array SiteOpt }
defaultSiteOpts = { xs: [] }
type SiteOpt = { id :: String, label :: Maybe String }
defaultSiteOpt :: { label :: Maybe String }
defaultSiteOpt = { label: Nothing }
type SiteOpt' = { id :: Maybe String, label :: Maybe String }
type Permissions = { xs :: Array String }
defaultPermissions :: { xs :: Array String }
defaultPermissions = { xs: [] }
type Page = { tpe :: PageType, guest :: Boolean, seo :: PageSeo, mobileSeo :: Maybe PageSeo, name :: Array (Tuple String String) }
defaultPage :: { mobileSeo :: Maybe PageSeo, name :: Array (Tuple String String) }
defaultPage = { mobileSeo: Nothing, name: [] }
type Page' = { tpe :: Maybe PageType, guest :: Maybe Boolean, seo :: Maybe PageSeo, mobileSeo :: Maybe PageSeo, name :: Array (Tuple String String) }
type PageUrl' = { addr :: Maybe String }
type PageSeo' = { descr :: Maybe String, order :: Maybe Number }
type PageTreeItem = { priority :: Int }
type PageTreeItem' = { priority :: Maybe Int }
type ComponentTemplateOk = { fieldNode :: FieldNode, fieldNode1 :: FieldNode1 }
type ComponentTemplateOk' = { fieldNode :: Maybe FieldNode, fieldNode1 :: Maybe FieldNode1 }
type FieldNode' = { root :: Maybe String, forest :: Array FieldNode }
newtype FieldNode1 = FieldNode1 { root :: Maybe String, forest :: Array FieldNode1 }
derive instance eqFieldNode1 :: Eq FieldNode1
defaultFieldNode1 :: { root :: Maybe String, forest :: Array FieldNode1 }
defaultFieldNode1 = { root: Nothing, forest: [] }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodeSiteOpts _xs_ pos1) SiteOpts
    2 -> decode (decodePermissions _xs_ pos1) Permissions
    3 -> decode (decodePage _xs_ pos1) Page
    4 -> decode (decodePageTreeItem _xs_ pos1) PageTreeItem
    5 -> decode (decodePing _xs_ pos1) \_ -> Ping
    1300 -> decode (decodeComponentTemplateOk _xs_ pos1) ComponentTemplateOk
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodeSiteOpts :: Uint8Array -> Int -> Decode.Result SiteOpts
decodeSiteOpts _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { xs: [] } pos
    where
    decode :: Int -> SiteOpts -> Int -> Decode.Result' (Step { a :: Int, b :: SiteOpts, c :: Int } { pos :: Int, val :: SiteOpts })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeSiteOpt _xs_ pos2) \val -> acc { xs = snoc acc.xs val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeSiteOpt :: Uint8Array -> Int -> Decode.Result SiteOpt
decodeSiteOpt _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { id: Nothing, label: Nothing } pos
  case val of
    { id: Just id, label } -> pure { pos: pos1, val: { id, label } }
    _ -> Left $ Decode.MissingFields "SiteOpt"
    where
    decode :: Int -> SiteOpt' -> Int -> Decode.Result' (Step { a :: Int, b :: SiteOpt', c :: Int } { pos :: Int, val :: SiteOpt' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { id = Just val }
        2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { label = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePermissions :: Uint8Array -> Int -> Decode.Result Permissions
decodePermissions _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { xs: [] } pos
    where
    decode :: Int -> Permissions -> Int -> Decode.Result' (Step { a :: Int, b :: Permissions, c :: Int } { pos :: Int, val :: Permissions })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { xs = snoc acc.xs val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePage :: Uint8Array -> Int -> Decode.Result Page
decodePage _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { tpe: Nothing, guest: Nothing, seo: Nothing, mobileSeo: Nothing, name: [] } pos
  case val of
    { tpe: Just tpe, guest: Just guest, seo: Just seo, mobileSeo, name } -> pure { pos: pos1, val: { tpe, guest, seo, mobileSeo, name } }
    _ -> Left $ Decode.MissingFields "Page"
    where
    decode :: Int -> Page' -> Int -> Decode.Result' (Step { a :: Int, b :: Page', c :: Int } { pos :: Int, val :: Page' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodePageType _xs_ pos2) \val -> acc { tpe = Just val }
        2 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { guest = Just val }
        3 -> decodeFieldLoop end (decodePageSeo _xs_ pos2) \val -> acc { seo = Just val }
        4 -> decodeFieldLoop end (decodePageSeo _xs_ pos2) \val -> acc { mobileSeo = Just val }
        5 -> decodeFieldLoop end (decodeStringString _xs_ pos2) \val -> acc { name = snoc acc.name val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePageType :: Uint8Array -> Int -> Decode.Result PageType
decodePageType _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) Nothing pos
    where
    decode :: Int -> Maybe PageType -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe PageType, c :: Int } { pos :: Int, val :: PageType })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodePageWidgets _xs_ pos2) \_ -> Just PageWidgets
        2 -> decodeFieldLoop end (decodePageUrl _xs_ pos2) (Just <<< PageUrl)
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end (Just acc) pos1 = pure $ Done { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "PageType"

decodePageWidgets :: Uint8Array -> Int -> Decode.Result Unit
decodePageWidgets _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodePageUrl :: Uint8Array -> Int -> Decode.Result PageUrl
decodePageUrl _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { addr: Nothing } pos
  case val of
    { addr: Just addr } -> pure { pos: pos1, val: { addr } }
    _ -> Left $ Decode.MissingFields "PageUrl"
    where
    decode :: Int -> PageUrl' -> Int -> Decode.Result' (Step { a :: Int, b :: PageUrl', c :: Int } { pos :: Int, val :: PageUrl' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { addr = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePageSeo :: Uint8Array -> Int -> Decode.Result PageSeo
decodePageSeo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { descr: Nothing, order: Nothing } pos
  case val of
    { descr: Just descr, order: Just order } -> pure { pos: pos1, val: { descr, order } }
    _ -> Left $ Decode.MissingFields "PageSeo"
    where
    decode :: Int -> PageSeo' -> Int -> Decode.Result' (Step { a :: Int, b :: PageSeo', c :: Int } { pos :: Int, val :: PageSeo' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { descr = Just val }
        2 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { order = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeStringString :: Uint8Array -> Int -> Decode.Result (Tuple String String)
decodeStringString _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { first: Nothing, second: Nothing } pos
  case val of
    { first: Just first, second: Just second } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $ Decode.MissingFields "decodeStringString"
    where
    decode :: Int -> { first :: Maybe String, second :: Maybe String } -> Int -> Decode.Result' (Step { a :: Int, b :: { first :: Maybe String, second :: Maybe String }, c :: Int } { pos :: Int, val :: { first :: Maybe String, second :: Maybe String } })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { first = Just val }
        2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { second = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) (\_ -> acc)
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePageTreeItem :: Uint8Array -> Int -> Decode.Result PageTreeItem
decodePageTreeItem _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { priority: Nothing } pos
  case val of
    { priority: Just priority } -> pure { pos: pos1, val: { priority } }
    _ -> Left $ Decode.MissingFields "PageTreeItem"
    where
    decode :: Int -> PageTreeItem' -> Int -> Decode.Result' (Step { a :: Int, b :: PageTreeItem', c :: Int } { pos :: Int, val :: PageTreeItem' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.signedVarint32 _xs_ pos2) \val -> acc { priority = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePing :: Uint8Array -> Int -> Decode.Result Unit
decodePing _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeComponentTemplateOk :: Uint8Array -> Int -> Decode.Result ComponentTemplateOk
decodeComponentTemplateOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { fieldNode: Nothing, fieldNode1: Nothing } pos
  case val of
    { fieldNode: Just fieldNode, fieldNode1: Just fieldNode1 } -> pure { pos: pos1, val: { fieldNode, fieldNode1 } }
    _ -> Left $ Decode.MissingFields "ComponentTemplateOk"
    where
    decode :: Int -> ComponentTemplateOk' -> Int -> Decode.Result' (Step { a :: Int, b :: ComponentTemplateOk', c :: Int } { pos :: Int, val :: ComponentTemplateOk' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeFieldNode _xs_ pos2) \val -> acc { fieldNode = Just val }
        2 -> decodeFieldLoop end (decodeFieldNode1 _xs_ pos2) \val -> acc { fieldNode1 = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFieldNode :: Uint8Array -> Int -> Decode.Result FieldNode
decodeFieldNode _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { root: Nothing, forest: [] } pos
  case val of
    { root: Just root, forest } -> pure { pos: pos1, val: FieldNode { root, forest } }
    _ -> Left $ Decode.MissingFields "FieldNode"
    where
    decode :: Int -> FieldNode' -> Int -> Decode.Result' (Step { a :: Int, b :: FieldNode', c :: Int } { pos :: Int, val :: FieldNode' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { root = Just val }
        2 -> decodeFieldLoop end (decodeFieldNode _xs_ pos2) \val -> acc { forest = snoc acc.forest val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFieldNode1 :: Uint8Array -> Int -> Decode.Result FieldNode1
decodeFieldNode1 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) (FieldNode1 { root: Nothing, forest: [] }) pos
    where
    decode :: Int -> FieldNode1 -> Int -> Decode.Result' (Step { a :: Int, b :: FieldNode1, c :: Int } { pos :: Int, val :: FieldNode1 })
    decode end acc'@(FieldNode1 acc) pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> FieldNode1 $ acc { root = Just val }
        2 -> decodeFieldLoop end (decodeFieldNode1 _xs_ pos2) \val -> FieldNode1 $ acc { forest = snoc acc.forest val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc'
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }