module Enum.Pull
  ( Pull(..)
  , encodePull
  ) where

import Data.Eq (class Eq)
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, concatAll)

data Pull = Ping
derive instance eqPull :: Eq Pull

encodePull :: Pull -> Uint8Array
encodePull Ping = concatAll [ Encode.unsignedVarint32 10, encodePing ]

encodePing :: Uint8Array
encodePing = Encode.unsignedVarint32 0