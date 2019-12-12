module SetMap.Common where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Set (Set)

type Flow1 = { graph :: Map String (Set String) }
type Flow2 = { graph :: Array (Tuple StepId (Array StepId)) }
data StepId = Prod | Dev


