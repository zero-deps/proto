module Common
  ( PageType(..)
  , PageUrl
  , PageSeo
  , FieldNode(FieldNode)
  , defaultFieldNode 
  ) where

import Data.Eq (class Eq)

data PageType = PageWidgets | PageUrl PageUrl
derive instance eqPageType :: Eq PageType
type PageUrl = { addr :: String }
type PageSeo = { descr :: String, order :: Number }
newtype FieldNode = FieldNode { root :: String, forest :: Array FieldNode }
derive instance eqFieldNode :: Eq FieldNode
defaultFieldNode :: { forest :: Array FieldNode }
defaultFieldNode = { forest: [] }