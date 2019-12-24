module SchemaCommon
  ( TestSchema(..)
  , ClassWithMap 
  ) where

import Data.Tuple (Tuple)

data TestSchema = ClassWithMap ClassWithMap
type ClassWithMap = { m :: Array (Tuple String String) }