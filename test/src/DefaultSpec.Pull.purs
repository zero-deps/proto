module DefaultSpec.Pull
  ( Pull(..)
  , encodePull
  ) where

import Data.Array (concatMap)
import Data.Eq (class Eq)
import Data.Maybe (fromMaybe)
import Prelude (map, ($))
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll, fromArray)
import DefaultSpec.Common

data Pull = SimpleT1 SimpleT1 | SimpleT2 SimpleT2 | RecursiveT1'' RecursiveT1 | RecursiveT2'' RecursiveT2 | OneMaybe OneMaybe | OneSeq OneSeq
derive instance eqPull :: Eq Pull

encodePull :: Pull -> Uint8Array
encodePull (SimpleT1 x) = concatAll [ Encode.unsignedVarint32 10, encodeSimpleT1 x ]
encodePull (SimpleT2 x) = concatAll [ Encode.unsignedVarint32 18, encodeSimpleT2 x ]
encodePull (RecursiveT1'' x) = concatAll [ Encode.unsignedVarint32 26, encodeRecursiveT1 x ]
encodePull (RecursiveT2'' x) = concatAll [ Encode.unsignedVarint32 34, encodeRecursiveT2 x ]
encodePull (OneMaybe x) = concatAll [ Encode.unsignedVarint32 42, encodeOneMaybe x ]
encodePull (OneSeq x) = concatAll [ Encode.unsignedVarint32 50, encodeOneSeq x ]

encodeSimpleT1 :: SimpleT1 -> Uint8Array
encodeSimpleT1 msg = do
  let xs = concatAll
        [ fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.unsignedVarint32 8, Encode.boolean x ]) msg.m1
        , Encode.unsignedVarint32 16
        , Encode.boolean msg.b1
        , Encode.unsignedVarint32 26
        , Encode.string msg.b2
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeSimpleT2 :: SimpleT2 -> Uint8Array
encodeSimpleT2 msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.boolean msg.b0
        , Encode.unsignedVarint32 16
        , Encode.boolean msg.b1
        , Encode.unsignedVarint32 26
        , Encode.string msg.b2
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeRecursiveT1 :: RecursiveT1 -> Uint8Array
encodeRecursiveT1 (RecursiveT1 msg) = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.boolean msg.b1
        , Encode.unsignedVarint32 18
        , Encode.string msg.b2
        , Encode.unsignedVarint32 26
        , encodeRecursiveT1 msg.x
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeRecursiveT2 :: RecursiveT2 -> Uint8Array
encodeRecursiveT2 (RecursiveT2 msg) = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.boolean msg.b1
        , Encode.unsignedVarint32 18
        , Encode.string msg.b2
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.unsignedVarint32 26, encodeRecursiveT2 x ]) msg.x
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeOneMaybe :: OneMaybe -> Uint8Array
encodeOneMaybe msg = do
  let xs = concatAll
        [ fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.unsignedVarint32 10, Encode.string x ]) msg.m1
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeOneSeq :: OneSeq -> Uint8Array
encodeOneSeq msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 10, Encode.string x ]) msg.xs
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]