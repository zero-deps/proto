module Proto.Decode where

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Int.Bits (shl, zshr, (.&.), (.|.))
import Prelude
import Uint8ArrayExt (length, slice)
import Uint8ArrayExt as Uint8Array
import Utf8 as Utf8

type Pos = Int
data Error
  = OutOfBound Int Int
  | BadWireType Int
  | BadType Int
  | UnexpectedCase Int Int
  | MissingFields String
  | IntTooLong
type Result a = Either Error { pos :: Int, val :: a }

foreign import joinFloat64 :: Int -> Int -> Number

instance showError :: Show Error where
  show (OutOfBound i l) = "index="<>show i<>" out of bound="<>show l
  show (BadWireType x) = "bad wire type="<>show x
  show (BadType x) = "bad type="<>show x
  show (UnexpectedCase x i) = "unexpected case val="<>show x<>" pos="<>show i
  show (MissingFields x) = "missing fields in="<>x
  show (IntTooLong) = "varint32 too long"

index :: Uint8Array -> Int -> Either Error Int
index xs pos = lmap (\x -> OutOfBound x.pos x.len) $ Uint8Array.index xs pos

int32 :: Uint8Array -> Pos -> Result Int
int32 = uint32

uint32 :: Uint8Array -> Pos -> Result Int
uint32 xs pos = do
  x <- index xs pos
  let val = (x .&. 127) `zshr` 0
  if x < 128 then pure { pos: pos+1, val: val } else do
    x1 <- index xs (pos+1)
    let val1 = (val .|. ((x1 .&. 127) `shl` 7)) `zshr` 0
    if x1 < 128 then pure { pos: pos+2, val: val1 } else do
      x2 <- index xs (pos+2)
      let val2 = (val1 .|. ((x2 .&. 127) `shl` 14)) `zshr` 0
      if x2 < 128 then pure { pos: pos+3, val: val2 } else do
        x3 <- index xs (pos+3)
        let val3 = (val2 .|. ((x3 .&. 127) `shl` 21)) `zshr` 0
        if x3 < 128 then pure { pos: pos+4, val: val3 } else do
          x4 <- index xs (pos+4)
          let val4 = val3 .|. ((x4 .&. 15) `shl` 28)
          if x4 < 128 then pure { pos: pos+5, val: val4 `zshr` 0 } else do
            let len = length xs
            x5 <- index xs (pos+5)
            if x5 < 128
              then pure { pos: pos+6, val }
              else do
                x6 <- index xs (pos+6)
                if x6 < 128
                  then pure { pos: pos+7, val }
                  else do
                    x7 <- index xs (pos+7)
                    if x7 < 128
                      then pure { pos: pos+8, val }
                      else do
                        x8 <- index xs (pos+8)
                        if x8 < 128
                          then pure { pos: pos+9, val }
                          else do
                            x9 <- index xs (pos+9)
                            if x9 < 128
                              then pure { pos: pos+10, val }
                              else Left $ IntTooLong

boolean :: Uint8Array -> Pos -> Result Boolean
boolean xs pos = do
  x <- index xs pos
  if x == 0 then pure { pos: pos+1, val: false } else pure { pos: pos+1, val: true }

double :: Uint8Array -> Pos -> Result Number
double xs pos = do
  { pos: pos2, val: bitsLow } <- fixedUint32 pos
  { pos: pos3, val: bitsHigh } <- fixedUint32 pos2
  pure { pos: pos3, val: joinFloat64 bitsLow bitsHigh }
  where
  fixedUint32 :: Pos -> Result Int
  fixedUint32 pos1 = do
    a <- index xs (pos1+0)
    b <- index xs (pos1+1)
    c <- index xs (pos1+2)
    d <- index xs (pos1+3)
    pure { pos: pos1+4, val: ((a `shl` 0) .|. (b `shl` 8) .|. (c `shl` 16) .|. (d `shl` 24)) `zshr` 0 }

bytes :: Uint8Array -> Pos -> Result Uint8Array
bytes xs pos0 = do
  { pos, val: res_len } <- uint32 xs pos0
  let start = pos
  let end = pos+res_len
  let len = length xs
  if end > len
    then Left (OutOfBound end len)
    else pure { pos: pos+res_len, val: slice xs start end }

string :: Uint8Array -> Pos -> Result String
string xs pos0 = do
  { pos, val: ys } <- bytes xs pos0
  pure { pos, val: Utf8.toString ys }

skip' :: Uint8Array -> Pos -> Result Unit
skip' xs pos =
  case index xs pos of
    Left x -> Left x
    Right x | x .&. 128 == 0 -> pure { pos: pos+1, val: unit }
    Right _ -> skip' xs (pos+1)

skip :: Int -> Uint8Array -> Pos -> Result Unit
skip n xs pos0 = let len = length xs in if pos0 + n > len then Left (OutOfBound (pos0+n) len) else pure { pos: pos0+n, val: unit }

skipType :: Uint8Array -> Pos -> Int -> Result Unit
skipType xs pos0 0 = skip' xs pos0
skipType xs pos0 1 = skip 8 xs pos0
skipType xs pos0 2 = do
  { pos, val } <- uint32 xs pos0
  skip val xs $ pos
skipType xs0 pos0 3 = loop xs0 pos0
  where
  loop xs pos = do
    { pos: pos1, val } <- uint32 xs pos
    let wireType = val .&. 7
    if wireType /= 4 then do
      { pos: pos2 } <- skipType xs pos1 wireType
      loop xs pos2
      else pure { pos: pos1, val: unit }
skipType xs pos0 5 = skip 4 xs pos0
skipType _ _ i = Left $ BadWireType i

