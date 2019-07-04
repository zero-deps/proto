module Common where
data PageType = PageWidgets PageWidgets | PageUrl PageUrl
type PageWidgets = {  }
type PageUrl = { addr :: String }
type PageSeo = { descr :: String, order :: Number }


