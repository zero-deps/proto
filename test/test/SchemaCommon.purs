module SchemaCommon
  ( TestSchema(..)
  , ClassWithMap
  , defaultClassWithMap 
  ) where

import Data.Eq (class Eq)
import Data.Tuple (Tuple)

data TestSchema = ClassWithMap ClassWithMap
derive instance eqTestSchema :: Eq TestSchema
type ClassWithMap = { m :: Array (Tuple String String) }
defaultClassWithMap :: { m :: Array (Tuple String String) }
defaultClassWithMap = { m: [] }