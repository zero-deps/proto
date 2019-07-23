module SchemaPull where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, ($))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll, fromArray)
import SchemaCommon

encodeStringString :: Tuple String String -> Uint8Array
encodeStringString (Tuple k v) = do
  let xs = concatAll [ Encode.uint32 10, Encode.string k, Encode.uint32 18, Encode.string v ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]



encodeTestSchema :: TestSchema -> Uint8Array
encodeTestSchema (ClassWithMap x) = concatAll [ Encode.uint32 10, encodeClassWithMap x ]

encodeClassWithMap :: ClassWithMap -> Uint8Array
encodeClassWithMap msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.uint32 10, encodeStringString x ]) $ Map.toUnfoldableUnordered msg.m
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]
