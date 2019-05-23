module Proto where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right))
import Data.Int.Bits (shl, zshr, (.&.), (.|.))
import Prelude

foreign import uint8array_length :: Uint8Array -> Int
foreign import uint8array_index_impl :: Uint8Array -> Int -> Int
foreign import uint8array_slice :: Uint8Array -> Int -> Int -> Uint8Array
foreign import uint8array_concatall :: Array Uint8Array -> Uint8Array
foreign import utf8_length :: String -> Int
foreign import utf8_read :: Uint8Array -> String
foreign import utf8_write :: String -> Int -> Uint8Array
foreign import array_touint8array :: Array Int -> Uint8Array

type Pos = Int
data Error
  = OutOfBound Int Int
  | BadWireType Int
  | BadType Int
  | UnexpectedCase Int Int
type Result a = Either Error { offset :: Int, val :: a }

instance showError :: Show Error where
  show (OutOfBound i l) = "index="<>show i<>" out of bound="<>show l
  show (BadWireType x) = "bad wire type="<>show x
  show (BadType x) = "bad type="<>show x
  show (UnexpectedCase x i) = "unexpected case val="<>show x<>" pos="<>show i

uint8array_index :: Uint8Array -> Int -> Either Error Int
uint8array_index xs i =
  let len = uint8array_length xs
  in if 0 <= i && i < len then Right (uint8array_index_impl xs i) else Left (OutOfBound i len)

read_uint32 :: Uint8Array -> Pos -> Result Int
read_uint32 xs pos = do
  val <- map (\x -> (x .&. 127) `zshr` 0) $ uint8array_index xs pos
  if val < 128 then pure { offset: 1, val: val } else do
    val1 <- map (\x -> (val .|. ((x .&. 127) `shl` 7)) `zshr` 0) $ uint8array_index xs (pos+1)
    if val1 < 128 then pure { offset: 2, val: val1 } else do
      val2 <- map (\x -> (val1 .|. ((x .&. 127) `shl` 14)) `zshr` 0) $ uint8array_index xs (pos+2)
      if val2 < 128 then pure { offset: 3, val: val2 } else do
        val3 <- map (\x -> (val2 .|. ((x .&. 127) `shl` 21)) `zshr` 0) $ uint8array_index xs (pos+3)
        if val3 < 128 then pure { offset: 4, val: val3 } else do
          val4 <- map (\x -> (val3 .|. ((x .&. 15) `shl` 28)) `zshr` 0) $ uint8array_index xs (pos+4)
          if val4 < 128 then pure { offset: 5, val: val4 } else Left $ UnexpectedCase val4 (pos+4)

read_bytes :: Uint8Array -> Pos -> Result Uint8Array
read_bytes xs pos = do
  { offset, val: res_len } <- read_uint32 xs pos
  let start = pos+offset
  let end = pos+offset+res_len
  let len = uint8array_length xs
  if end > len
    then Left (OutOfBound end len)
    else pure { offset: offset+res_len, val: uint8array_slice xs start end }

read_string :: Uint8Array -> Pos -> Result String
read_string xs pos = do
  { offset, val: ys } <- read_bytes xs pos
  pure { offset, val: utf8_read ys }

skip' :: Uint8Array -> Pos -> Result Unit
skip' xs pos0 = let len = uint8array_length xs in loop pos0 len
  where
  loop :: Pos -> Int -> Result Unit
  loop pos len =
    if pos >= len then Left (OutOfBound pos len)
    else if ((uint8array_index_impl xs pos) .&. 128) == 0 then pure { offset: 1, val: unit }
    else loop (pos+1) len

skip :: Int -> Uint8Array -> Pos -> Result Unit
skip n xs pos0 = let len = uint8array_length xs in if pos0 + n > len then Left (OutOfBound (pos0+n) len) else pure { offset: pos0+n, val: unit }

skipType :: Uint8Array -> Pos -> Int -> Result Unit
skipType xs pos 0 = skip' xs pos
skipType xs pos 1 = skip 8 xs pos
skipType xs pos 2 = do
  { offset, val } <- read_uint32 xs pos
  skip val xs $ pos+offset
skipType xs0 pos0 3 = loop xs0 pos0
  where
  loop xs pos = do
    { offset, val } <- read_uint32 xs pos
    let wireType = val .&. 7
    if wireType /= 4 then do
      { offset: offset1 } <- skipType xs (pos+offset) wireType
      loop xs $ pos+offset+offset1
      else pure { offset: pos+offset, val: unit }
skipType xs pos 5 = skip 4 xs pos
skipType _ _ x = Left $ BadWireType x

write_uint32 :: Int -> Uint8Array
write_uint32 = loop [] >>> array_touint8array
  where
  loop :: Array Int -> Int -> Array Int
  loop acc val = if val > 127 then loop (acc `snoc` ((val .&. 127) .|. 128)) (val `zshr` 7) else (acc `snoc` val)

write_string :: String -> Uint8Array
write_string x = do
  let len = utf8_length x
  if len == 0
    then write_uint32 0
    else uint8array_concatall [ write_uint32 len, utf8_write x len ]

write_bytes :: Uint8Array -> Uint8Array
write_bytes xs = do
  let len = uint8array_length xs
  if len == 0
    then write_uint32 0
    else uint8array_concatall [ write_uint32 len, xs ]

