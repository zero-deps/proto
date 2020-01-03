module EqSpec.Pull
  ( Pull(..)
  , encodePull
  ) where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Prelude (($))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll)
import EqSpec.Common

data Pull = Flow Flow

encodePull :: Pull -> Uint8Array
encodePull (Flow x) = concatAll [ Encode.uint32 10, encodeFlow x ]

encodeFlow :: Flow -> Uint8Array
encodeFlow msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.uint32 10, encodeFlowStep x ]) msg.steps
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeFlowStep :: FlowStep -> Uint8Array
encodeFlowStep Start = do
  let xs = concatAll [ Encode.uint32 10, encodeStart ]
  concatAll [ Encode.uint32 $ length xs, xs ]
encodeFlowStep (Ext x) = do
  let xs = concatAll [ Encode.uint32 18, encodeExt x ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeStart :: Uint8Array
encodeStart = Encode.uint32 0

encodeExt :: Ext -> Uint8Array
encodeExt msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , encodeNode msg.tree
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeNode :: Node -> Uint8Array
encodeNode (Node msg) = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.root
        , concatAll $ concatMap (\x -> [ Encode.uint32 18, encodeNode x ]) msg.forest
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]