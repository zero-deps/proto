module SchemaPull
  ( encodeTestSchema
  ) where

import Data.Array (concatMap)
import Data.Tuple (Tuple(Tuple))
import Prelude (($))
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)
import SchemaCommon



encodeTestSchema :: TestSchema -> Uint8Array
encodeTestSchema (ClassWithMap x) = concatAll [ Encode.unsignedVarint32 10, encodeClassWithMap x ]
encodeTestSchema (ClassWithLong x) = concatAll [ Encode.unsignedVarint32 18, encodeClassWithLong x ]
encodeTestSchema (ClassWithInt x) = concatAll [ Encode.unsignedVarint32 26, encodeClassWithInt x ]

encodeClassWithMap :: ClassWithMap -> Uint8Array
encodeClassWithMap msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 10, encodeStringString x ]) msg.m
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

encodeClassWithLong :: ClassWithLong -> Uint8Array
encodeClassWithLong msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.bigInt msg.x
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeClassWithInt :: ClassWithInt -> Uint8Array
encodeClassWithInt msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.signedVarint32 msg.x
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]