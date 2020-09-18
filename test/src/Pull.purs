module Pull
  ( Pull(..)
  , UploadChunk
  , defaultUploadChunk
  , SavePage
  , defaultSavePage
  , SaveComponentTemplate
  , ComponentsSavePrefs
  , defaultComponentsSavePrefs
  , encodePull
  ) where

import Data.Array (concatMap)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, ($))
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll, fromArray)
import Common

data Pull = GetSites | UploadChunk UploadChunk | SavePage SavePage | SaveComponentTemplate SaveComponentTemplate | ComponentsSavePrefs ComponentsSavePrefs
derive instance eqPull :: Eq Pull
type UploadChunk = { path :: Array String, id :: String, chunk :: Uint8Array }
defaultUploadChunk :: { path :: Array String }
defaultUploadChunk = { path: [] }
type SavePage = { tpe :: PageType, guest :: Boolean, seo :: PageSeo, mobileSeo :: Maybe PageSeo, name :: Array (Tuple String String) }
defaultSavePage :: { mobileSeo :: Maybe PageSeo, name :: Array (Tuple String String) }
defaultSavePage = { mobileSeo: Nothing, name: [] }
type SaveComponentTemplate = { fieldNode :: FieldNode }
type ComponentsSavePrefs = { id :: String, pageid :: String, siteid :: String, tree :: FieldNode, extTree :: Maybe FieldNode }
defaultComponentsSavePrefs :: { extTree :: Maybe FieldNode }
defaultComponentsSavePrefs = { extTree: Nothing }

encodePull :: Pull -> Uint8Array
encodePull GetSites = concatAll [ Encode.unsignedVarint32 8002, encodeGetSites ]
encodePull (UploadChunk x) = concatAll [ Encode.unsignedVarint32 8010, encodeUploadChunk x ]
encodePull (SavePage x) = concatAll [ Encode.unsignedVarint32 8018, encodeSavePage x ]
encodePull (SaveComponentTemplate x) = concatAll [ Encode.unsignedVarint32 11202, encodeSaveComponentTemplate x ]
encodePull (ComponentsSavePrefs x) = concatAll [ Encode.unsignedVarint32 15362, encodeComponentsSavePrefs x ]

encodeGetSites :: Uint8Array
encodeGetSites = Encode.unsignedVarint32 0

encodeUploadChunk :: UploadChunk -> Uint8Array
encodeUploadChunk msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 10, Encode.string x ]) msg.path
        , Encode.unsignedVarint32 18
        , Encode.string msg.id
        , Encode.unsignedVarint32 26
        , Encode.bytes msg.chunk
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeSavePage :: SavePage -> Uint8Array
encodeSavePage msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , encodePageType msg.tpe
        , Encode.unsignedVarint32 16
        , Encode.boolean msg.guest
        , Encode.unsignedVarint32 26
        , encodePageSeo msg.seo
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.unsignedVarint32 34, encodePageSeo x ]) msg.mobileSeo
        , concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 42, encodeStringString x ]) msg.name
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodePageType :: PageType -> Uint8Array
encodePageType PageWidgets = do
  let xs = concatAll [ Encode.unsignedVarint32 10, encodePageWidgets ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodePageType (PageUrl x) = do
  let xs = concatAll [ Encode.unsignedVarint32 18, encodePageUrl x ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodePageWidgets :: Uint8Array
encodePageWidgets = Encode.unsignedVarint32 0

encodePageUrl :: PageUrl -> Uint8Array
encodePageUrl msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.addr
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodePageSeo :: PageSeo -> Uint8Array
encodePageSeo msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.descr
        , Encode.unsignedVarint32 17
        , Encode.double msg.order
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeStringString :: Tuple String String -> Uint8Array
encodeStringString (Tuple _1 _2) = do
  let msg = { _1, _2 }
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg._1
        , Encode.unsignedVarint32 18
        , Encode.string msg._2
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeSaveComponentTemplate :: SaveComponentTemplate -> Uint8Array
encodeSaveComponentTemplate msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , encodeFieldNode msg.fieldNode
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeFieldNode :: FieldNode -> Uint8Array
encodeFieldNode (FieldNode msg) = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.root
        , concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 18, encodeFieldNode x ]) msg.forest
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeComponentsSavePrefs :: ComponentsSavePrefs -> Uint8Array
encodeComponentsSavePrefs msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.id
        , Encode.unsignedVarint32 18
        , Encode.string msg.pageid
        , Encode.unsignedVarint32 26
        , Encode.string msg.siteid
        , Encode.unsignedVarint32 34
        , encodeFieldNode msg.tree
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.unsignedVarint32 42, encodeFieldNode x ]) msg.extTree
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]