module Utf8 where

import Data.ArrayBuffer.Types (Uint8Array)

foreign import numOfBytes :: String -> Int
foreign import toString :: Uint8Array -> String
foreign import toUint8Array :: String -> Int -> Uint8Array

