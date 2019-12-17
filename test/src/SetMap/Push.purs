module SetMap.Push where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left, Right))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Unit (Unit, unit)
import Prelude (bind, pure, ($), (+), (<))
import Proto.Decode as Decode
import SetMap.Common

data Push = Flow1 Flow1 | Flow2 Flow2
type Flow1' = { graph :: Array (Tuple String (Array String)) }
type Flow2' = { graph :: Array (Tuple StepId (Array StepId)) }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> do
      { pos: pos2, val } <- decodeFlow1 _xs_ pos1
      pure { pos: pos2, val: Flow1 val }
    2 -> do
      { pos: pos2, val } <- decodeFlow2 _xs_ pos1
      pure { pos: pos2, val: Flow2 val }
    i ->
      Left $ Decode.BadType i

decodeFlow1 :: Uint8Array -> Int -> Decode.Result Flow1
decodeFlow1 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { graph: [] } pos
  case val of
    { graph } -> pure { pos: pos1, val: { graph } }
    where
    decode :: Int -> Flow1' -> Int -> Decode.Result Flow1'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodeStringArrayString _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { graph = snoc acc.graph val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodeStringArrayString :: Uint8Array -> Int -> Decode.Result (Tuple String (Array String))
decodeStringArrayString _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { first: Nothing, second: [] } pos
  case val of
    { first: Just first, second } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $ Decode.MissingFields "decodeStringArrayString"
    where
    decode :: Int -> { first :: Maybe String, second :: Array String } -> Int -> Decode.Result { first :: Maybe String, second :: Array String }
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { first = Just val }) pos3
              2 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { second = snoc acc.second val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodeFlow2 :: Uint8Array -> Int -> Decode.Result Flow2
decodeFlow2 _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { graph: [] } pos
  case val of
    { graph } -> pure { pos: pos1, val: { graph } }
    where
    decode :: Int -> Flow2' -> Int -> Decode.Result Flow2'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodeStepIdArrayStepId _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { graph = snoc acc.graph val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodeStepIdArrayStepId :: Uint8Array -> Int -> Decode.Result (Tuple StepId (Array StepId))
decodeStepIdArrayStepId _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { first: Nothing, second: [] } pos
  case val of
    { first: Just first, second } -> pure { pos: pos1, val: Tuple first second }
    _ -> Left $ Decode.MissingFields "decodeStepIdArrayStepId"
    where
    decode :: Int -> { first :: Maybe StepId, second :: Array StepId } -> Int -> Decode.Result { first :: Maybe StepId, second :: Array StepId }
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodeStepId _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { first = Just val }) pos3
              2 ->
                case decodeStepId _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { second = snoc acc.second val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }

decodeStepId :: Uint8Array -> Int -> Decode.Result StepId
decodeStepId _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  decode end Nothing pos
    where
    decode :: Int -> Maybe StepId -> Int -> Decode.Result StepId
    decode end acc pos1 | pos1 < end =
      case Decode.uint32 _xs_ pos1 of
        Left x -> Left x
        Right { pos: pos2, val: tag } ->
          case tag `zshr` 3 of
            1 ->
              case decodeProd _xs_ pos2 of
                Left x -> Left x
                Right { pos: pos3 } -> decode end (Just Prod) pos3
            2 ->
              case decodeDev _xs_ pos2 of
                Left x -> Left x
                Right { pos: pos3 } -> decode end (Just Dev) pos3
            _ ->
              case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                Left x -> Left x
                Right { pos: pos3 } ->
                  decode end acc pos3
    decode end (Just acc) pos1 = pure { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "StepId"

decodeProd :: Uint8Array -> Int -> Decode.Result Unit
decodeProd _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  pure { pos: end, val: unit }

decodeDev :: Uint8Array -> Int -> Decode.Result Unit
decodeDev _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  pure { pos: end, val: unit }
