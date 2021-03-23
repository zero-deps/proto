module Enum.Push
  ( Push(..)
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done))
import Data.Either (Either(Left))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr)
import Data.Unit (Unit, unit)
import Prelude (map, bind, pure, ($), (+))
import Proto.Decode as Decode
import Proto.Uint8Array (Uint8Array)


decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = Pong
derive instance eqPush :: Eq Push

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodePong _xs_ pos1) \_ -> Pong
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodePong :: Uint8Array -> Int -> Decode.Result Unit
decodePong _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }