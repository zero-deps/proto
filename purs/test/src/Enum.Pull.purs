module Enum.Pull
  ( Pull(..)
  , Save
  , encodePull
  ) where

import Data.Eq (class Eq)
import Prelude (($))
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)


data Pull = Ping | Save Save
derive instance eqPull :: Eq Pull
type Save = { x :: String }

encodePull :: Pull -> Uint8Array
encodePull Ping = concatAll [ Encode.unsignedVarint32 10, encodePing ]
encodePull (Save x) = concatAll [ Encode.unsignedVarint32 18, encodeSave x ]

encodePing :: Uint8Array
encodePing = Encode.unsignedVarint32 0

encodeSave :: Save -> Uint8Array
encodeSave msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.x
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]