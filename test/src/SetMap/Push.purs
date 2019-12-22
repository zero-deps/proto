module SetMap.Push
  ( Push(..)
  , decodePush
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
import Prelude (map, bind, pure, ($), (+), (<), (<<<))
import Proto.Decode as Decode
import SetMap.Common

decodeField :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeField end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = Flow1 Flow1 | Flow2 Flow2

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodeFlow1 _xs_ pos1) Flow1
    2 -> decode (decodeFlow2 _xs_ pos1) Flow2
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodeFlow1 :: Uint8Array -> Int -> Decode.Result Flow1
decodeFlow1 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { graph: [] } pos
    where
    decode :: Int -> Flow1 -> Int -> Decode.Result' (Step { a :: Int, b :: Flow1, c :: Int } { pos :: Int, val :: Flow1 })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeField end (decodeStringArrayString _xs_ pos2) \val -> acc { graph = snoc acc.graph val }
        _ -> do
          { pos: pos3 } <- Decode.skipType _xs_ pos2 $ tag .&. 7
          pure $ Loop { a: end, b: acc, c: pos3 }
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeStringArrayString :: Uint8Array -> Int -> Decode.Result (Tuple String (Array String))
decodeStringArrayString _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { first: Nothing, second: [] } pos
  case val of
    { first: Just first, second } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $ Decode.MissingFields "decodeStringArrayString"
    where
    decode :: Int -> { first :: Maybe String, second :: Array String } -> Int -> Decode.Result' (Step { a :: Int, b :: { first :: Maybe String, second :: Array String }, c :: Int } { pos :: Int, val :: { first :: Maybe String, second :: Array String } })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeField end (Decode.string _xs_ pos2) \val -> acc { first = Just val }
        2 -> decodeField end (Decode.string _xs_ pos2) \val -> acc { second = snoc acc.second val }
        _ -> do
          { pos: pos3 } <- Decode.skipType _xs_ pos2 $ tag .&. 7
          pure $ Loop { a: end, b: acc, c: pos3 }
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFlow2 :: Uint8Array -> Int -> Decode.Result Flow2
decodeFlow2 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { graph: [] } pos
    where
    decode :: Int -> Flow2 -> Int -> Decode.Result' (Step { a :: Int, b :: Flow2, c :: Int } { pos :: Int, val :: Flow2 })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeField end (decodeStepIdArrayStepId _xs_ pos2) \val -> acc { graph = snoc acc.graph val }
        _ -> do
          { pos: pos3 } <- Decode.skipType _xs_ pos2 $ tag .&. 7
          pure $ Loop { a: end, b: acc, c: pos3 }
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeStepIdArrayStepId :: Uint8Array -> Int -> Decode.Result (Tuple StepId (Array StepId))
decodeStepIdArrayStepId _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { first: Nothing, second: [] } pos
  case val of
    { first: Just first, second } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $ Decode.MissingFields "decodeStepIdArrayStepId"
    where
    decode :: Int -> { first :: Maybe StepId, second :: Array StepId } -> Int -> Decode.Result' (Step { a :: Int, b :: { first :: Maybe StepId, second :: Array StepId }, c :: Int } { pos :: Int, val :: { first :: Maybe StepId, second :: Array StepId } })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeField end (decodeStepId _xs_ pos2) \val -> acc { first = Just val }
        2 -> decodeField end (decodeStepId _xs_ pos2) \val -> acc { second = snoc acc.second val }
        _ -> do
          { pos: pos3 } <- Decode.skipType _xs_ pos2 $ tag .&. 7
          pure $ Loop { a: end, b: acc, c: pos3 }
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeStepId :: Uint8Array -> Int -> Decode.Result StepId
decodeStepId _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) Nothing pos
    where
    decode :: Int -> Maybe StepId -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe StepId, c :: Int } { pos :: Int, val :: StepId })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> do
          { pos: pos3 } <- decodeProd _xs_ pos2
          pure $ Loop { a: end, b: Just Prod, c: pos3 }
        2 -> do
          { pos: pos3 } <- decodeDev _xs_ pos2
          pure $ Loop { a: end, b: Just Dev, c: pos3 }
        _ -> do
          { pos: pos3 } <- Decode.skipType _xs_ pos2 $ tag .&. 7
          pure $ Loop { a: end, b: acc, c: pos3 }
    decode end (Just acc) pos1 = pure $ Done { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "StepId"

decodeProd :: Uint8Array -> Int -> Decode.Result Unit
decodeProd _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeDev :: Uint8Array -> Int -> Decode.Result Unit
decodeDev _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }