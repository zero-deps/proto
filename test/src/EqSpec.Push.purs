module EqSpec.Push
  ( Push(..)
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Array (snoc)
import Data.Either (Either(Left))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Prelude (map, bind, pure, ($), (+), (<), (<<<))
import Proto.Decode as Decode
import Proto.Uint8Array (Uint8Array)
import EqSpec.Common

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = Flow Flow
derive instance eqPush :: Eq Push
type Ext' = { tree :: Maybe Node }
type Node' = { root :: Maybe String, forest :: Array Node }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodeFlow _xs_ pos1) Flow
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodeFlow :: Uint8Array -> Int -> Decode.Result Flow
decodeFlow _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { steps: [] } pos
    where
    decode :: Int -> Flow -> Int -> Decode.Result' (Step { a :: Int, b :: Flow, c :: Int } { pos :: Int, val :: Flow })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeFlowStep _xs_ pos2) \val -> acc { steps = snoc acc.steps val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFlowStep :: Uint8Array -> Int -> Decode.Result FlowStep
decodeFlowStep _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) Nothing pos
    where
    decode :: Int -> Maybe FlowStep -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe FlowStep, c :: Int } { pos :: Int, val :: FlowStep })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeStart _xs_ pos2) \_ -> Just Start
        2 -> decodeFieldLoop end (decodeExt _xs_ pos2) (Just <<< Ext)
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end (Just acc) pos1 = pure $ Done { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "FlowStep"

decodeStart :: Uint8Array -> Int -> Decode.Result Unit
decodeStart _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeExt :: Uint8Array -> Int -> Decode.Result Ext
decodeExt _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { tree: Nothing } pos
  case val of
    { tree: Just tree } -> pure { pos: pos1, val: { tree } }
    _ -> Left $ Decode.MissingFields "Ext"
    where
    decode :: Int -> Ext' -> Int -> Decode.Result' (Step { a :: Int, b :: Ext', c :: Int } { pos :: Int, val :: Ext' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeNode _xs_ pos2) \val -> acc { tree = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeNode :: Uint8Array -> Int -> Decode.Result Node
decodeNode _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { root: Nothing, forest: [] } pos
  case val of
    { root: Just root, forest } -> pure { pos: pos1, val: Node { root, forest } }
    _ -> Left $ Decode.MissingFields "Node"
    where
    decode :: Int -> Node' -> Int -> Decode.Result' (Step { a :: Int, b :: Node', c :: Int } { pos :: Int, val :: Node' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { root = Just val }
        2 -> decodeFieldLoop end (decodeNode _xs_ pos2) \val -> acc { forest = snoc acc.forest val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }