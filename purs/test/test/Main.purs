module Test.Main where

import Prelude (class Eq, Unit, discard, ($), (<>), (==))
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
  encode_map_in_scala_and_purs
  encode_maxlong_in_scala_and_purs
  encode_minlong_in_scala_and_purs
  encode_maxbigint_in_scala_and_purs
  encode_minbigint_in_scala_and_purs
  encode_maxint_in_scala_and_purs
  encode_minint_in_scala_and_purs

encode_decode_map :: Effect Unit
encode_decode_map = do
  let xs = encodeTestSchema Cases.map_schema
  case decodeTestSchema xs of
    Left err -> do
      log $ show err
      exit 1
    Right { val } -> assertEqual val Cases.map_schema "encode_decode_map"

encode_map_in_scala_and_purs :: Effect Unit
encode_map_in_scala_and_purs = do
  let xs = encodeTestSchema Cases.map_schema
  let actual = printBytes xs
  let expected = Cases.map_bytestr
  assertEqual actual expected "encode_map_in_scala_and_purs"

encode_maxlong_in_scala_and_purs :: Effect Unit
encode_maxlong_in_scala_and_purs = do
  let xs = encodeTestSchema Cases.maxlong_schema
  let actual = printBytes xs
  let expected = Cases.maxlong_bytestr
  assertEqual actual expected "encode_maxlong_in_scala_and_purs"

encode_minlong_in_scala_and_purs :: Effect Unit
encode_minlong_in_scala_and_purs = do
  let xs = encodeTestSchema Cases.minlong_schema
  let actual = printBytes xs
  let expected = Cases.minlong_bytestr
  assertEqual actual expected "encode_minlong_in_scala_and_purs"

encode_maxbigint_in_scala_and_purs :: Effect Unit
encode_maxbigint_in_scala_and_purs = do
  let xs = encodeTestSchema Cases.max_bigint_schema
  let actual = printBytes xs
  let expected = Cases.max_bigint_bytestr
  assertEqual actual expected "encode_maxbigint_in_scala_and_purs"

encode_minbigint_in_scala_and_purs :: Effect Unit
encode_minbigint_in_scala_and_purs = do
  let xs = encodeTestSchema Cases.min_bigint_schema
  let actual = printBytes xs
  let expected = Cases.min_bigint_bytestr
  assertEqual actual expected "encode_minbigint_in_scala_and_purs"

encode_maxint_in_scala_and_purs :: Effect Unit
encode_maxint_in_scala_and_purs = do
  let xs = encodeTestSchema Cases.maxint_schema
  let actual = printBytes xs
  let expected = Cases.maxint_bytestr
  assertEqual actual expected "encode_maxint_in_scala_and_purs"

encode_minint_in_scala_and_purs :: Effect Unit
encode_minint_in_scala_and_purs = do
  let xs = encodeTestSchema Cases.minint_schema
  let actual = printBytes xs
  let expected = Cases.minint_bytestr
  assertEqual actual expected "encode_minint_in_scala_and_purs"

assertEqual :: forall a. Eq a => a -> a -> String -> Effect Unit
assertEqual actual expected msg =
  if actual == expected then log $ msg <> ": ok"
  else do
    log $ msg <> ": bad"
    log $ "  actual: " <> show actual
    log $ "expected: " <> show expected
    exit 1

foreign import printBytes :: Uint8Array -> String

foreign import show :: forall a. a -> String