module Pull where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, ($))
import Proto.Encode as Encode
import Uint8ArrayExt (length, concatAll, fromArray)
import Common

encodeStringString :: Tuple String String -> Uint8Array
encodeStringString (Tuple k v) = concatAll [ Encode.uint32 10, Encode.string k, Encode.uint32 18, Encode.string v ]

data Pull = GetSites GetSites | UploadChunk UploadChunk | SavePage SavePage
type GetSites = {  }
type UploadChunk = { path :: Array String, id :: String, chunk :: Uint8Array }
type SavePage = { tpe :: PageType, guest :: Boolean, seo :: PageSeo, mobileSeo :: Maybe PageSeo, name :: Map String String }

encodePull :: Pull -> Uint8Array
encodePull (GetSites x) = concatAll [ Encode.uint32 8002, encodeGetSites x ]
encodePull (UploadChunk x) = concatAll [ Encode.uint32 8010, encodeUploadChunk x ]
encodePull (SavePage x) = concatAll [ Encode.uint32 8018, encodeSavePage x ]

encodeGetSites :: GetSites -> Uint8Array
encodeGetSites _ = Encode.uint32 0

encodeUploadChunk :: UploadChunk -> Uint8Array
encodeUploadChunk msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.uint32 10, Encode.string x ]) msg.path
        , Encode.uint32 18
        , Encode.string msg.id
        , Encode.uint32 26
        , Encode.bytes msg.chunk
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeSavePage :: SavePage -> Uint8Array
encodeSavePage msg = do
  let xs = concatAll
        [ encodePageType msg.tpe
        , Encode.uint32 16
        , Encode.boolean msg.guest
        , encodePageSeo msg.seo
        , fromMaybe (fromArray []) $ map encodePageSeo msg.mobileSeo
        , concatAll $ map encodeStringString $ Map.toUnfoldableUnordered msg.name
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodePageType :: PageType -> Uint8Array
encodePageType (PageWidgets x) = concatAll [ Encode.uint32 10, encodePageWidgets x ]
encodePageType (PageUrl x) = concatAll [ Encode.uint32 18, encodePageUrl x ]

encodePageWidgets :: PageWidgets -> Uint8Array
encodePageWidgets _ = Encode.uint32 0

encodePageUrl :: PageUrl -> Uint8Array
encodePageUrl msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.addr
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodePageSeo :: PageSeo -> Uint8Array
encodePageSeo msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.descr
        , Encode.uint32 17
        , Encode.double msg.order
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]
