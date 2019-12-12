module SchemaCommon where

import Data.Eq (class Eq)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Tuple (Tuple)

data TestSchema = ClassWithMap ClassWithMap
type ClassWithMap = { m :: Map String String }


