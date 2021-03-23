module Enum.Push
  ( Push(..)
  , Saved
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Either (Either(Left))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Prelude (map, bind, pure, ($), (+), (<))
import Proto.Decode as Decode
import Proto.Uint8Array (Uint8Array)


decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = Pong | Saved Saved
derive instance eqPush :: Eq Push
type Saved = { x :: String }
type Saved' = { x :: Maybe String }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodePong _xs_ pos1) \_ -> Pong
    2 -> decode (decodeSaved _xs_ pos1) Saved
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodePong :: Uint8Array -> Int -> Decode.Result Unit
decodePong _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeSaved :: Uint8Array -> Int -> Decode.Result Saved
decodeSaved _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { x: Nothing } pos
  case val of
    { x: Just x } -> pure { pos: pos1, val: { x } }
    _ -> Left $ Decode.MissingFields "Saved"
    where
    decode :: Int -> Saved' -> Int -> Decode.Result' (Step { a :: Int, b :: Saved', c :: Int } { pos :: Int, val :: Saved' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { x = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }