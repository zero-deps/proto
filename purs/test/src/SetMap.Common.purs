module SetMap.Common
  ( Flow1
  , defaultFlow1
  , Flow2
  , defaultFlow2
  , StepId(..) 
  ) where

import Data.Eq (class Eq)
import Data.Tuple (Tuple)

type Flow1 = { graph :: Array (Tuple String (Array String)) }
defaultFlow1 :: { graph :: Array (Tuple String (Array String)) }
defaultFlow1 = { graph: [] }
type Flow2 = { graph :: Array (Tuple StepId (Array StepId)) }
defaultFlow2 :: { graph :: Array (Tuple StepId (Array StepId)) }
defaultFlow2 = { graph: [] }
data StepId = Prod | Dev
derive instance eqStepId :: Eq StepId