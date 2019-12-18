module SchemaPush
  ( decodeTestSchema
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit, unit)
import Prelude (bind, pure, ($), (+), (<))
import Proto.Decode as Decode
import SchemaCommon

type ClassWithMap' = { m :: Array (Tuple String String) }

decodeTestSchema :: Uint8Array -> Decode.Result TestSchema
decodeTestSchema _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> do
      { pos: pos2, val } <- decodeClassWithMap _xs_ pos1
      pure { pos: pos2, val: ClassWithMap val }
    i ->
      Left $ Decode.BadType i

decodeClassWithMap :: Uint8Array -> Int -> Decode.Result ClassWithMap
decodeClassWithMap _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- tailRecM3 decode end { m: [] } pos
  case val of
    { m } -> pure { pos: pos1, val: { m } }
    where
    decode :: Int -> ClassWithMap' -> Int -> Decode.Result' (Step { a :: Int, b :: ClassWithMap', c :: Int } { pos :: Int, val :: ClassWithMap' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> do
          { pos: pos3, val } <- decodeStringString _xs_ pos2
          pure $ Loop { a: end, b: acc { m = snoc acc.m val }, c: pos3 }
        _ -> do
          { pos: pos3 } <- Decode.skipType _xs_ pos2 $ tag .&. 7
          pure $ Loop { a: end, b: acc, c: pos3 }
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeStringString :: Uint8Array -> Int -> Decode.Result (Tuple String String)
decodeStringString _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- tailRecM3 decode end { first: Nothing, second: Nothing } pos
  case val of
    { first: Just first, second: Just second } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $ Decode.MissingFields "decodeStringString"
    where
    decode :: Int -> { first :: Maybe String, second :: Maybe String } -> Int -> Decode.Result' (Step { a :: Int, b :: { first :: Maybe String, second :: Maybe String }, c :: Int } { pos :: Int, val :: { first :: Maybe String, second :: Maybe String } })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> do
          { pos: pos3, val } <- Decode.string _xs_ pos2
          pure $ Loop { a: end, b: acc { first = Just val }, c: pos3 }
        2 -> do
          { pos: pos3, val } <- Decode.string _xs_ pos2
          pure $ Loop { a: end, b: acc { second = Just val }, c: pos3 }
        _ -> do
          { pos: pos3 } <- Decode.skipType _xs_ pos2 $ tag .&. 7
          pure $ Loop { a: end, b: acc, c: pos3 }
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }