module DefaultSpec.Pull
  ( Pull(..)
  , encodePull
  ) where

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (fromMaybe)
import Prelude (map, ($))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll, fromArray)
import DefaultSpec.Common

data Pull = SimpleT1 SimpleT1 | SimpleT2 SimpleT2 | RecursiveT1'' RecursiveT1 | RecursiveT2'' RecursiveT2

encodePull :: Pull -> Uint8Array
encodePull (SimpleT1 x) = concatAll [ Encode.uint32 10, encodeSimpleT1 x ]
encodePull (SimpleT2 x) = concatAll [ Encode.uint32 18, encodeSimpleT2 x ]
encodePull (RecursiveT1'' x) = concatAll [ Encode.uint32 26, encodeRecursiveT1 x ]
encodePull (RecursiveT2'' x) = concatAll [ Encode.uint32 26, encodeRecursiveT2 x ]

encodeSimpleT1 :: SimpleT1 -> Uint8Array
encodeSimpleT1 msg = do
  let xs = concatAll
        [ fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.uint32 8, Encode.boolean x ]) msg.m1
        , Encode.uint32 16
        , Encode.boolean msg.b1
        , Encode.uint32 24
        , Encode.boolean msg.b2
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeSimpleT2 :: SimpleT2 -> Uint8Array
encodeSimpleT2 msg = do
  let xs = concatAll
        [ Encode.uint32 8
        , Encode.boolean msg.b0
        , Encode.uint32 16
        , Encode.boolean msg.b1
        , Encode.uint32 24
        , Encode.boolean msg.b2
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeRecursiveT1 :: RecursiveT1 -> Uint8Array
encodeRecursiveT1 (RecursiveT1 msg) = do
  let xs = concatAll
        [ Encode.uint32 8
        , Encode.boolean msg.b1
        , Encode.uint32 16
        , Encode.boolean msg.b2
        , Encode.uint32 26
        , encodeRecursiveT1 msg.x
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeRecursiveT2 :: RecursiveT2 -> Uint8Array
encodeRecursiveT2 (RecursiveT2 msg) = do
  let xs = concatAll
        [ Encode.uint32 8
        , Encode.boolean msg.b1
        , Encode.uint32 16
        , Encode.boolean msg.b2
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.uint32 26, encodeRecursiveT2 x ]) msg.x
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]