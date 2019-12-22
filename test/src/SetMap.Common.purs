module SetMap.Common
  ( Flow1
  , Flow2
  , StepId(..) 
  ) where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type Flow1 = { graph :: Array (Tuple String (Array String)) }
type Flow2 = { graph :: Array (Tuple StepId (Array StepId)) }
data StepId = Prod | Dev
derive instance eqStepId :: Eq StepId