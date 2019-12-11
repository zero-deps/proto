module SchemaCommon where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Set (Set)

data TestSchema = ClassWithMap ClassWithMap
type ClassWithMap = { m :: Map String String }


