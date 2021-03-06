module SetMap.Pull
  ( Pull(..)
  , encodePull
  ) where

import Data.Array (concatMap)
import Data.Eq (class Eq)
import Data.Tuple (Tuple(Tuple))
import Prelude (($))
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)
import SetMap.Common

data Pull = Flow1 Flow1 | Flow2 Flow2
derive instance eqPull :: Eq Pull

encodePull :: Pull -> Uint8Array
encodePull (Flow1 x) = concatAll [ Encode.unsignedVarint32 10, encodeFlow1 x ]
encodePull (Flow2 x) = concatAll [ Encode.unsignedVarint32 18, encodeFlow2 x ]

encodeFlow1 :: Flow1 -> Uint8Array
encodeFlow1 msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 10, encodeStringArrayString x ]) msg.graph
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeStringArrayString :: Tuple String (Array String) -> Uint8Array
encodeStringArrayString (Tuple _1 _2) = do
  let msg = { _1, _2 }
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg._1
        , concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 18, Encode.string x ]) msg._2
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeFlow2 :: Flow2 -> Uint8Array
encodeFlow2 msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 10, encodeStepIdArrayStepId x ]) msg.graph
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeStepIdArrayStepId :: Tuple StepId (Array StepId) -> Uint8Array
encodeStepIdArrayStepId (Tuple _1 _2) = do
  let msg = { _1, _2 }
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , encodeStepId msg._1
        , concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 18, encodeStepId x ]) msg._2
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeStepId :: StepId -> Uint8Array
encodeStepId Prod = do
  let xs = concatAll [ Encode.unsignedVarint32 10, encodeProd ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeStepId Dev = do
  let xs = concatAll [ Encode.unsignedVarint32 18, encodeDev ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeProd :: Uint8Array
encodeProd = Encode.unsignedVarint32 0

encodeDev :: Uint8Array
encodeDev = Encode.unsignedVarint32 0