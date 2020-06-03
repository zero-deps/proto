module DefaultSpec.Push
  ( Push(..)
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Array (snoc)
import Data.Either (Either(Left))
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Prelude (map, bind, pure, ($), (+), (<))
import Proto.Decode as Decode
import Proto.Uint8Array (Uint8Array)
import DefaultSpec.Common

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = SimpleT1 SimpleT1 | SimpleT2 SimpleT2 | RecursiveT1'' RecursiveT1 | RecursiveT2'' RecursiveT2 | OneMaybe OneMaybe | OneSeq OneSeq
type SimpleT1' = { m1 :: Maybe Boolean, b1 :: Maybe Boolean, b2 :: Maybe String }
type SimpleT2' = { b0 :: Maybe Boolean, b1 :: Maybe Boolean, b2 :: Maybe String }
type RecursiveT1' = { b1 :: Maybe Boolean, b2 :: Maybe String, x :: Maybe RecursiveT1 }
type RecursiveT2' = { b1 :: Maybe Boolean, b2 :: Maybe String, x :: Maybe RecursiveT2 }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodeSimpleT1 _xs_ pos1) SimpleT1
    2 -> decode (decodeSimpleT2 _xs_ pos1) SimpleT2
    3 -> decode (decodeRecursiveT1 _xs_ pos1) RecursiveT1''
    4 -> decode (decodeRecursiveT2 _xs_ pos1) RecursiveT2''
    5 -> decode (decodeOneMaybe _xs_ pos1) OneMaybe
    6 -> decode (decodeOneSeq _xs_ pos1) OneSeq
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodeSimpleT1 :: Uint8Array -> Int -> Decode.Result SimpleT1
decodeSimpleT1 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val: { m1, b1, b2 } } <- tailRecM3 decode (pos + msglen) { m1: Nothing, b1: Nothing, b2: Nothing } pos
  pure { pos: pos1, val: { m1, b1: fromMaybe false b1, b2: fromMaybe "" b2 }}
    where
    decode :: Int -> SimpleT1' -> Int -> Decode.Result' (Step { a :: Int, b :: SimpleT1', c :: Int } { pos :: Int, val :: SimpleT1' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { m1 = Just val }
        2 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b1 = Just val }
        3 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { b2 = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeSimpleT2 :: Uint8Array -> Int -> Decode.Result SimpleT2
decodeSimpleT2 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { b0: Nothing, b1: Nothing, b2: Nothing } pos
  case val of
    { b0: Just b0, b1, b2 } -> pure { pos: pos1, val: { b0, b1: fromMaybe false b1, b2: fromMaybe "" b2 }}
    _ -> Left $ Decode.MissingFields "SimpleT2"
    where
    decode :: Int -> SimpleT2' -> Int -> Decode.Result' (Step { a :: Int, b :: SimpleT2', c :: Int } { pos :: Int, val :: SimpleT2' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b0 = Just val }
        2 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b1 = Just val }
        3 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { b2 = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeRecursiveT1 :: Uint8Array -> Int -> Decode.Result RecursiveT1
decodeRecursiveT1 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { b1: Nothing, b2: Nothing, x: Nothing } pos
  case val of
    { b1, b2, x: Just x } -> pure { pos: pos1, val: RecursiveT1 { b1: fromMaybe false b1, b2: fromMaybe "" b2, x }}
    _ -> Left $ Decode.MissingFields "RecursiveT1"
    where
    decode :: Int -> RecursiveT1' -> Int -> Decode.Result' (Step { a :: Int, b :: RecursiveT1', c :: Int } { pos :: Int, val :: RecursiveT1' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b1 = Just val }
        2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { b2 = Just val }
        3 -> decodeFieldLoop end (decodeRecursiveT1 _xs_ pos2) \val -> acc { x = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeRecursiveT2 :: Uint8Array -> Int -> Decode.Result RecursiveT2
decodeRecursiveT2 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val: { b1, b2, x } } <- tailRecM3 decode (pos + msglen) { b1: Nothing, b2: Nothing, x: Nothing } pos
  pure { pos: pos1, val: RecursiveT2 { b1: fromMaybe false b1, b2: fromMaybe "" b2, x }}
    where
    decode :: Int -> RecursiveT2' -> Int -> Decode.Result' (Step { a :: Int, b :: RecursiveT2', c :: Int } { pos :: Int, val :: RecursiveT2' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b1 = Just val }
        2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { b2 = Just val }
        3 -> decodeFieldLoop end (decodeRecursiveT2 _xs_ pos2) \val -> acc { x = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeOneMaybe :: Uint8Array -> Int -> Decode.Result OneMaybe
decodeOneMaybe _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { m1: Nothing } pos
    where
    decode :: Int -> OneMaybe -> Int -> Decode.Result' (Step { a :: Int, b :: OneMaybe, c :: Int } { pos :: Int, val :: OneMaybe })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { m1 = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeOneSeq :: Uint8Array -> Int -> Decode.Result OneSeq
decodeOneSeq _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { xs: [] } pos
    where
    decode :: Int -> OneSeq -> Int -> Decode.Result' (Step { a :: Int, b :: OneSeq, c :: Int } { pos :: Int, val :: OneSeq })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { xs = snoc acc.xs val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }