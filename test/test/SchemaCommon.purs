module SchemaCommon
  ( TestSchema(..)
  , ClassWithMap
  , defaultClassWithMap
  , ClassWithLong
  , ClassWithInt 
  ) where

import Data.Eq (class Eq)
import Data.Tuple (Tuple)

data TestSchema = ClassWithMap ClassWithMap | ClassWithLong ClassWithLong | ClassWithInt ClassWithInt
derive instance eqTestSchema :: Eq TestSchema
type ClassWithMap = { m :: Array (Tuple String String) }
defaultClassWithMap :: { m :: Array (Tuple String String) }
defaultClassWithMap = { m: [] }
type ClassWithLong = { x :: Number }
type ClassWithInt = { x :: Int }