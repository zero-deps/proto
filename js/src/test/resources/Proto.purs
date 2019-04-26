module Proto where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Prelude

foreign import data Reader :: Type

foreign import createReader :: Uint8Array -> Reader
foreign import len :: Reader -> Int
foreign import pos :: Reader -> Int
foreign import uint32 :: Reader -> Effect Int
foreign import string :: Reader -> Effect String
foreign import skipType :: Reader -> Int -> Effect Unit
  
data Push
  = SiteOpts SiteOpts

type SiteOpts =
  { xs :: Array SiteOpt
  }

type SiteOpt =
  { id :: String
  , label :: String
  }

decodePush :: Uint8Array -> Effect (Maybe Push)
decodePush bytes = do
  let reader = createReader bytes
  tag <- uint32 reader
  case zshr tag 3 of
    1 -> do
      msglen <- uint32 reader
      x <- decodeSiteOpts reader msglen
      pure $ Just $ SiteOpts x
    _ ->
      pure Nothing

decodeSiteOpts :: Reader -> Int -> Effect SiteOpts
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
      else pure acc
  
decodeSiteOpt :: Reader -> Int -> Effect SiteOpt
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
      else pure acc
