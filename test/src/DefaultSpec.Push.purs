module DefaultSpec.Push
  ( Push(..)
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left))
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Prelude (map, bind, pure, ($), (+), (<))
import Proto.Decode as Decode
import DefaultSpec.Common

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = SimpleT1 SimpleT1 | SimpleT2 SimpleT2 | PRecursiveT RecursiveT
type SimpleT1' = { m1 :: Maybe Boolean, b1 :: Maybe Boolean, b2 :: Maybe Boolean }
type SimpleT2' = { b0 :: Maybe Boolean, b1 :: Maybe Boolean, b2 :: Maybe Boolean }
type RecursiveT' = { b1 :: Maybe Boolean, b2 :: Maybe Boolean, x :: Maybe RecursiveT }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodeSimpleT1 _xs_ pos1) SimpleT1
    2 -> decode (decodeSimpleT2 _xs_ pos1) SimpleT2
    3 -> decode (decodeRecursiveT _xs_ pos1) PRecursiveT
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodeSimpleT1 :: Uint8Array -> Int -> Decode.Result SimpleT1
decodeSimpleT1 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val: { m1, b1, b2 } } <- tailRecM3 decode (pos + msglen) { m1: Nothing, b1: Nothing, b2: Nothing } pos
  pure { pos: pos1, val: { m1, b1: fromMaybe false b1, b2: fromMaybe true b2 } }
    where
    decode :: Int -> SimpleT1' -> Int -> Decode.Result' (Step { a :: Int, b :: SimpleT1', c :: Int } { pos :: Int, val :: SimpleT1' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { m1 = Just val }
        2 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b1 = Just val }
        3 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b2 = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeSimpleT2 :: Uint8Array -> Int -> Decode.Result SimpleT2
decodeSimpleT2 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { b0: Nothing, b1: Nothing, b2: Nothing } pos
  case val of
    { b0: Just b0, b1, b2 } -> pure { pos: pos1, val: { b0, b1: fromMaybe false b1,  b2: fromMaybe true b2 } }
    _ -> Left $ Decode.MissingFields "SimpleT2"
    where
    decode :: Int -> SimpleT2' -> Int -> Decode.Result' (Step { a :: Int, b :: SimpleT2', c :: Int } { pos :: Int, val :: SimpleT2' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b0 = Just val }
        2 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b1 = Just val }
        3 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b2 = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeRecursiveT :: Uint8Array -> Int -> Decode.Result RecursiveT
decodeRecursiveT _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { b1: Nothing, b2: Nothing, x: Nothing } pos
  case val of
    { b1, b2, x: Just x } -> pure { pos: pos1, val: RecursiveT { b1: fromMaybe false b1,  b2: fromMaybe true b2, x } }
    _ -> Left $ Decode.MissingFields "RecursiveT"
    where
    decode :: Int -> RecursiveT' -> Int -> Decode.Result' (Step { a :: Int, b :: RecursiveT', c :: Int } { pos :: Int, val :: RecursiveT' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b1 = Just val }
        2 -> decodeFieldLoop end (Decode.boolean _xs_ pos2) \val -> acc { b2 = Just val }
        3 -> decodeFieldLoop end (decodeRecursiveT _xs_ pos2) \val -> acc { x = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }