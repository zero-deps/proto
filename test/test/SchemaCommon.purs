module SchemaCommon
  ( TestSchema(..)
  , ClassWithMap
  , defaultClassWithMap 
  ) where

import Data.Tuple (Tuple)

data TestSchema = ClassWithMap ClassWithMap
type ClassWithMap = { m :: Array (Tuple String String) }
defaultClassWithMap :: { m :: Array (Tuple String String) }
defaultClassWithMap = { m: [] }