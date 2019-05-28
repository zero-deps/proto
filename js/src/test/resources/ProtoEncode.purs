module Proto.Encode where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Int.Bits (zshr, (.&.), (.|.))
import Prelude
import Uint8ArrayExt (length, concatAll, fromArray)
import Utf8 as Utf8

uint32 :: Int -> Uint8Array
uint32 = loop [] >>> fromArray
  where
  loop :: Array Int -> Int -> Array Int
  loop acc val = if val > 127 then loop (acc `snoc` ((val .&. 127) .|. 128)) (val `zshr` 7) else (acc `snoc` val)

string :: String -> Uint8Array
string x = do
  let len = Utf8.numOfBytes x
  if len == 0
    then uint32 0
    else concatAll [ uint32 len, Utf8.toUint8Array x len ]

bytes :: Uint8Array -> Uint8Array
bytes xs = do
  let len = length xs
  if len == 0
    then uint32 0
    else concatAll [ uint32 len, xs ]

