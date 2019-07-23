module SchemaCommon where

import Data.Map (Map)
import Data.Maybe (Maybe)

data TestSchema = ClassWithMap ClassWithMap
type ClassWithMap = { m :: Map String String }


