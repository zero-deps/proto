module Common where

import Data.Map (Map)
import Data.Maybe (Maybe)

data PageType = PageWidgets PageWidgets | PageUrl PageUrl
type PageWidgets = {  }
type PageUrl = { addr :: String }
type PageSeo = { descr :: String, order :: Number }
newtype FieldNode = FieldNode { root :: String, forest :: Array FieldNode }


