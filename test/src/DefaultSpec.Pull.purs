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

data Pull = SimpleT1 SimpleT1 | SimpleT2 SimpleT2 | RecursiveT'' RecursiveT

encodePull :: Pull -> Uint8Array
encodePull (SimpleT1 x) = concatAll [ Encode.uint32 10, encodeSimpleT1 x ]
encodePull (SimpleT2 x) = concatAll [ Encode.uint32 18, encodeSimpleT2 x ]
encodePull (RecursiveT'' x) = concatAll [ Encode.uint32 26, encodeRecursiveT x ]

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

encodeRecursiveT :: RecursiveT -> Uint8Array
encodeRecursiveT (RecursiveT msg) = do
  let xs = concatAll
        [ Encode.uint32 8
        , Encode.boolean msg.b1
        , Encode.uint32 16
        , Encode.boolean msg.b2
        , Encode.uint32 26
        , encodeRecursiveT msg.x
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]