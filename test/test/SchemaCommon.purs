module SchemaCommon
  ( TestSchema(..)
  , ClassWithMap
  ) where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

data TestSchema = ClassWithMap ClassWithMap
type ClassWithMap = { m :: Array (Tuple String String) }

