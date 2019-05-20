module Proto where

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import Prelude
import Data.Int.Bits (shl, zshr, (.&.), (.|.))
import Data.Either (Either(Left, Right))

type Pos = Int
data Error
  = OutOfBound Int Int
  | BadWireType Int
  | BadType Int
type Result a = Either Error { offset :: Int, val :: a }
foreign import arrayview_length :: Uint8Array -> Int
foreign import arrayview_index_impl :: Uint8Array -> Int -> Int

instance showError :: Show Error where
  show (OutOfBound i l) = "index="<>show i<>" out of bound="<>show l
  show (BadWireType x) = "bad wire type="<>show x
  show (BadType x) = "bad type="<>show x

arrayview_index :: Uint8Array -> Int -> Either Error Int
arrayview_index xs i =
  let len = arrayview_length xs
  in if 0 <= i && i < len then Right (arrayview_index_impl xs i) else Left (OutOfBound i len)

foreign import arrayview_slice :: Uint8Array -> Int -> Int -> Uint8Array
foreign import uint8array_tostring :: Uint8Array -> String

read_uint32 :: Uint8Array -> Pos -> Result Int
read_uint32 xs pos = do
  val <- map (\x -> (x .&. 127) `zshr` 0) $ arrayview_index xs pos
  if val < 128 then pure { offset: 1, val: val } else do
    val1 <- map (\x -> (val .|. ((x .&. 127) `shl` 7)) `zshr` 0) $ arrayview_index xs (pos+1)
    if val1 < 128 then pure { offset: 2, val: val1 } else do
      val2 <- map (\x -> (val1 .|. ((x .&. 127) `shl` 14)) `zshr` 0) $ arrayview_index xs (pos+2)
      if val2 < 128 then pure { offset: 3, val: val2 } else do
        val3 <- map (\x -> (val2 .|. ((x .&. 127) `shl` 21)) `zshr` 0) $ arrayview_index xs (pos+3)
        if val3 < 128 then pure { offset: 4, val: val3 } else do
          val4 <- map (\x -> (val3 .|. ((x .&. 15) `shl` 28)) `zshr` 0) $  arrayview_index xs (pos+4)
          if val4 < 128 then pure { offset: 5, val: val4 } else (let len = arrayview_length xs in if pos+10 > len then Left (OutOfBound (pos+9) len) else pure { offset: 10, val: val4 })

read_bytes :: Uint8Array -> Pos -> Result Uint8Array
read_bytes xs pos = do
  { offset, val: res_len } <- read_uint32 xs pos
  let start = pos+offset
  let end = pos+offset+res_len
  let len = arrayview_length xs
  if end > len
    then Left (OutOfBound end len)
    else pure { offset: offset+res_len, val: arrayview_slice xs start end }

read_string :: Uint8Array -> Pos -> Result String
read_string xs pos = do
  { offset, val: ys } <- read_bytes xs pos
  pure { offset, val: uint8array_tostring ys }

skip' :: Uint8Array -> Pos -> Result Unit
skip' xs pos0 = let len = arrayview_length xs in loop pos0 len
  where
  loop :: Pos -> Int -> Result Unit
  loop pos len =
    if pos >= len then Left (OutOfBound pos len)
    else if ((arrayview_index_impl xs pos) .&. 128) == 0 then pure { offset: 1, val: unit }
    else loop (pos+1) len

skip :: Int -> Uint8Array -> Pos -> Result Unit
skip n xs pos0 = let len = arrayview_length xs in if pos0 + n > len then Left (OutOfBound (pos0+n) len) else pure { offset: pos0+n, val: unit }

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

foreign import data Writer :: Type
foreign import createWriter :: Unit -> Writer
foreign import write_uint32 :: Writer -> Int -> Effect Unit
foreign import write_string :: Writer -> String -> Effect Unit
foreign import write_bytes :: Writer -> Uint8Array -> Effect Unit
foreign import writer_fork :: Writer -> Effect Unit
foreign import writer_ldelim :: Writer -> Effect Unit
foreign import writer_finish :: Writer -> Uint8Array

