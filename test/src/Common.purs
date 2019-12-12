module Common where

import Data.Eq (class Eq)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Tuple (Tuple)

data PageType = PageWidgets | PageUrl PageUrl
derive instance eqPageType :: Eq PageType
type PageUrl = { addr :: String }
type PageSeo = { descr :: String, order :: Number }
newtype FieldNode = FieldNode { root :: String, forest :: Array FieldNode }


