module SetMap.Pull where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Eq (class Eq)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, ($))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll, fromArray)
import SetMap.Common

data Pull = Flow1 Flow1 | Flow2 Flow2

encodePull :: Pull -> Uint8Array
encodePull (Flow1 x) = concatAll [ Encode.uint32 10, encodeFlow1 x ]
encodePull (Flow2 x) = concatAll [ Encode.uint32 18, encodeFlow2 x ]

encodeFlow1 :: Flow1 -> Uint8Array
encodeFlow1 msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.uint32 10, encodeStringArrayString x ]) msg.graph
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeStringArrayString :: Tuple String (Array String) -> Uint8Array
encodeStringArrayString (Tuple _1 _2) = do
  let msg = { _1, _2 }
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg._1
        , concatAll $ concatMap (\x -> [ Encode.uint32 18, Encode.string x ]) msg._2
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeFlow2 :: Flow2 -> Uint8Array
encodeFlow2 msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.uint32 10, encodeStepIdArrayStepId x ]) msg.graph
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeStepIdArrayStepId :: Tuple StepId (Array StepId) -> Uint8Array
encodeStepIdArrayStepId (Tuple _1 _2) = do
  let msg = { _1, _2 }
  let xs = concatAll
        [ Encode.uint32 10
        , encodeStepId msg._1
        , concatAll $ concatMap (\x -> [ Encode.uint32 18, encodeStepId x ]) msg._2
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeStepId :: StepId -> Uint8Array
encodeStepId Prod = do
  let xs = concatAll [ Encode.uint32 10, encodeProd ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]
encodeStepId Dev = do
  let xs = concatAll [ Encode.uint32 18, encodeDev ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

encodeProd :: Uint8Array
encodeProd = Encode.uint32 0

encodeDev :: Uint8Array
encodeDev = Encode.uint32 0