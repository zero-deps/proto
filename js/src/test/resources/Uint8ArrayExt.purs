module Uint8ArrayExt where

import Data.ArrayBuffer.Types (Uint8Array)

foreign import length :: Uint8Array -> Int
foreign import indexUnsafe :: Uint8Array -> Int -> Int
foreign import slice :: Uint8Array -> Int -> Int -> Uint8Array
foreign import concatAll :: Array Uint8Array -> Uint8Array
foreign import fromArray :: Array Int -> Uint8Array

