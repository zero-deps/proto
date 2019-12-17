module Test.Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, pure, show, unit, ($), (<>), (==))
import SchemaPull (encodeTestSchema)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right))
import Data.Tuple (Tuple(Tuple))
import Cases as Cases
import Node.Process (exit)
import Proto.Decode as Decode

main :: Effect Unit
main = do
  -- case1
  -- log "case1: ok"
  case2
  log "case2: ok"

-- case1 :: Effect Unit
-- case1 = do
--   let x = Tuple "a" "b"
--   let enc = encodeStringString x
--   let dec = decodeStringString enc 0
--   case (dec :: Decode.Result (Tuple String String)) of
--     Left err -> do
--       log $ show err
--       exit 1
--     Right { val } -> assertEqual x val

case2 :: Effect Unit
case2 = do
  let xs = encodeTestSchema Cases.c1
  let actual = printBytes xs
  let expected = Cases.r1
  assertEqual actual expected

assertEqual :: forall a. Eq a => Show a => a -> a -> Effect Unit
assertEqual actual expected =
  if actual == expected then pure unit
  else do
    log $ "  actual: " <> show actual
    log $ "expected: " <> show expected
    exit 1

foreign import printBytes :: Uint8Array -> String

