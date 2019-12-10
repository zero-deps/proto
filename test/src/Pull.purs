module Pull where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, ($))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll, fromArray)
import Common

data Pull = GetSites GetSites | UploadChunk UploadChunk | SavePage SavePage | SaveComponentTemplate SaveComponentTemplate | ComponentsSavePrefs ComponentsSavePrefs
type GetSites = {  }
type UploadChunk = { path :: Array String, id :: String, chunk :: Uint8Array }
type SavePage = { tpe :: PageType, guest :: Boolean, seo :: PageSeo, mobileSeo :: Maybe PageSeo, name :: Map String String }
type SaveComponentTemplate = { fieldNode :: FieldNode }
type ComponentsSavePrefs = { id :: String, pageid :: String, siteid :: String, tree :: FieldNode, extTree :: Maybe FieldNode }

encodePull :: Pull -> Uint8Array
encodePull (GetSites x) = concatAll [ Encode.uint32 8002, encodeGetSites x ]
encodePull (UploadChunk x) = concatAll [ Encode.uint32 8010, encodeUploadChunk x ]
encodePull (SavePage x) = concatAll [ Encode.uint32 8018, encodeSavePage x ]
encodePull (SaveComponentTemplate x) = concatAll [ Encode.uint32 11202, encodeSaveComponentTemplate x ]
encodePull (ComponentsSavePrefs x) = concatAll [ Encode.uint32 15362, encodeComponentsSavePrefs x ]

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
        [ Encode.uint32 10
        , encodePageType msg.tpe
        , Encode.uint32 16
        , Encode.boolean msg.guest
        , Encode.uint32 26
        , encodePageSeo msg.seo
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.uint32 34, encodePageSeo x ]) msg.mobileSeo
        , concatAll $ concatMap (\x -> [ Encode.uint32 42, encodeStringString x ]) $ Map.toUnfoldableUnordered msg.name
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodePageType :: PageType -> Uint8Array
encodePageType (PageWidgets x) = do
  let xs = concatAll [ Encode.uint32 10, encodePageWidgets x ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]
encodePageType (PageUrl x) = do
  let xs = concatAll [ Encode.uint32 18, encodePageUrl x ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

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

encodeStringString :: Tuple String String -> Uint8Array
encodeStringString (Tuple _1 _2) = do
  let msg = { _1, _2 }
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg._1
        , Encode.uint32 18
        , Encode.string msg._2
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeSaveComponentTemplate :: SaveComponentTemplate -> Uint8Array
encodeSaveComponentTemplate msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , encodeFieldNode msg.fieldNode
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeFieldNode :: FieldNode -> Uint8Array
encodeFieldNode (FieldNode msg) = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.root
        , concatAll $ concatMap (\x -> [ Encode.uint32 18, encodeFieldNode x ]) msg.forest
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeComponentsSavePrefs :: ComponentsSavePrefs -> Uint8Array
encodeComponentsSavePrefs msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.id
        , Encode.uint32 18
        , Encode.string msg.pageid
        , Encode.uint32 26
        , Encode.string msg.siteid
        , Encode.uint32 34
        , encodeFieldNode msg.tree
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.uint32 42, encodeFieldNode x ]) msg.extTree
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]
