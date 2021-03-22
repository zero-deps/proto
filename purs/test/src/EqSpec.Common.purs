module EqSpec.Common
  ( Flow
  , defaultFlow
  , FlowStep(..)
  , Ext
  , Node(Node)
  , defaultNode 
  ) where

import Data.Eq (class Eq)

type Flow = { steps :: Array FlowStep }
defaultFlow :: { steps :: Array FlowStep }
defaultFlow = { steps: [] }
data FlowStep = Start | Ext Ext
derive instance eqFlowStep :: Eq FlowStep
type Ext = { tree :: Node }
newtype Node = Node { root :: String, forest :: Array Node }
derive instance eqNode :: Eq Node
defaultNode :: { forest :: Array Node }
defaultNode = { forest: [] }