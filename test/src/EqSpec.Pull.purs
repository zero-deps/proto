module EqSpec.Pull
  ( Pull(..)
  , B
  , A(..)
  , C
  , encodePull
  ) where

import Data.Array (concatMap)
import Data.Eq (class Eq)
import Prelude (($))
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)
import EqSpec.Common

data Pull = Flow Flow | B B
derive instance eqPull :: Eq Pull
type B = { a :: A }
data A = C C
derive instance eqA :: Eq A
type C = { bytes :: Uint8Array }

encodePull :: Pull -> Uint8Array
encodePull (Flow x) = concatAll [ Encode.unsignedVarint32 10, encodeFlow x ]
encodePull (B x) = concatAll [ Encode.unsignedVarint32 18, encodeB x ]

encodeFlow :: Flow -> Uint8Array
encodeFlow msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 10, encodeFlowStep x ]) msg.steps
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeFlowStep :: FlowStep -> Uint8Array
encodeFlowStep Start = do
  let xs = concatAll [ Encode.unsignedVarint32 10, encodeStart ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeFlowStep (Ext x) = do
  let xs = concatAll [ Encode.unsignedVarint32 18, encodeExt x ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeStart :: Uint8Array
encodeStart = Encode.unsignedVarint32 0

encodeExt :: Ext -> Uint8Array
encodeExt msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , encodeNode msg.tree
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeNode :: Node -> Uint8Array
encodeNode (Node msg) = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.root
        , concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 18, encodeNode x ]) msg.forest
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeB :: B -> Uint8Array
encodeB msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , encodeA msg.a
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeA :: A -> Uint8Array
encodeA (C x) = do
  let xs = concatAll [ Encode.unsignedVarint32 10, encodeC x ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeC :: C -> Uint8Array
encodeC msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.bytes msg.bytes
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]