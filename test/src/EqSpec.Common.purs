module EqSpec.Common
  ( Flow
  , FlowStep(..)
  , Ext
  , Node(Node) 
  ) where

import Data.Eq (class Eq)

type Flow = { steps :: Array FlowStep }
data FlowStep = Start | Ext Ext
derive instance eqFlowStep :: Eq FlowStep
type Ext = { tree :: Node }
newtype Node = Node { root :: String, forest :: Array Node }
derive instance eqNode :: Eq Node