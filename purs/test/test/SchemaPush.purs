module SchemaPush
  ( decodeTestSchema
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Array (snoc)
import Data.Either (Either(Left))
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Prelude (map, bind, pure, ($), (+), (<))
import Proto.BigInt (BigInt)
import Proto.Decode as Decode
import Proto.Uint8Array (Uint8Array)
import SchemaCommon

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

type ClassWithLong' = { x :: Maybe BigInt }
type ClassWithInt' = { x :: Maybe Int }

decodeTestSchema :: Uint8Array -> Decode.Result TestSchema
decodeTestSchema _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodeClassWithMap _xs_ pos1) ClassWithMap
    2 -> decode (decodeClassWithLong _xs_ pos1) ClassWithLong
    3 -> decode (decodeClassWithInt _xs_ pos1) ClassWithInt
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> TestSchema) -> Decode.Result TestSchema
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodeClassWithMap :: Uint8Array -> Int -> Decode.Result ClassWithMap
decodeClassWithMap _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { m: [] } pos
    where
    decode :: Int -> ClassWithMap -> Int -> Decode.Result' (Step { a :: Int, b :: ClassWithMap, c :: Int } { pos :: Int, val :: ClassWithMap })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeStringString _xs_ pos2) \val -> acc { m = snoc acc.m val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeStringString :: Uint8Array -> Int -> Decode.Result (Tuple String String)
decodeStringString _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { first: Nothing, second: Nothing } pos
  case val of
    { first: Just first, second: Just second } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $ Decode.MissingFields "decodeStringString"
    where
    decode :: Int -> { first :: Maybe String, second :: Maybe String } -> Int -> Decode.Result' (Step { a :: Int, b :: { first :: Maybe String, second :: Maybe String }, c :: Int } { pos :: Int, val :: { first :: Maybe String, second :: Maybe String } })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { first = Just val }
        2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { second = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) (\_ -> acc)
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeClassWithLong :: Uint8Array -> Int -> Decode.Result ClassWithLong
decodeClassWithLong _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { x: Nothing } pos
  case val of
    { x: Just x } -> pure { pos: pos1, val: { x } }
    _ -> Left $ Decode.MissingFields "ClassWithLong"
    where
    decode :: Int -> ClassWithLong' -> Int -> Decode.Result' (Step { a :: Int, b :: ClassWithLong', c :: Int } { pos :: Int, val :: ClassWithLong' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.bigInt _xs_ pos2) \val -> acc { x = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeClassWithInt :: Uint8Array -> Int -> Decode.Result ClassWithInt
decodeClassWithInt _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { x: Nothing } pos
  case val of
    { x: Just x } -> pure { pos: pos1, val: { x } }
    _ -> Left $ Decode.MissingFields "ClassWithInt"
    where
    decode :: Int -> ClassWithInt' -> Int -> Decode.Result' (Step { a :: Int, b :: ClassWithInt', c :: Int } { pos :: Int, val :: ClassWithInt' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.signedVarint32 _xs_ pos2) \val -> acc { x = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }