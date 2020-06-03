module Test.Main where

import Prelude (class Eq, Unit, discard, pure, unit, ($), (<>), (==))
import Effect (Effect)
import Effect.Console (log)
import Node.Process (exit)

import Data.Either (Either(Left, Right))
import Proto.Uint8Array (Uint8Array)

import Cases as Cases
import SchemaPull (encodeTestSchema)
import SchemaPush (decodeTestSchema)

main :: Effect Unit
main = do
  encode_decode_map
  log "encode_decode_map: ok"
  encode_map_in_scala_and_purs
  log "encode_map_in_scala_and_purs: ok"

encode_decode_map :: Effect Unit
encode_decode_map = do
  let xs = encodeTestSchema Cases.c1
  case decodeTestSchema xs of
    Left err -> do
      log $ show err
      exit 1
    Right { val } -> assertEqual val Cases.c1

encode_map_in_scala_and_purs :: Effect Unit
encode_map_in_scala_and_purs = do
  let xs = encodeTestSchema Cases.c1
  let actual = printBytes xs
  let expected = Cases.r1
  assertEqual actual expected

assertEqual :: forall a. Eq a => a -> a -> Effect Unit
assertEqual actual expected =
  if actual == expected then pure unit
  else do
    log $ "  actual: " <> show actual
    log $ "expected: " <> show expected
    exit 1

foreign import printBytes :: Uint8Array -> String

foreign import show :: forall a. a -> String