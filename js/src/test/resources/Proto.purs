module Proto where

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import Prelude

foreign import data Reader :: Type

foreign import createReader :: Uint8Array -> Reader
foreign import len :: Reader -> Int
foreign import pos :: Reader -> Int
foreign import uint32 :: Reader -> Effect Int
foreign import string :: Reader -> Effect String
foreign import skipType :: Reader -> Int -> Effect Unit

foreign import data Writer :: Type

foreign import createWriter :: Unit -> Writer
foreign import write_uint32 :: Writer -> Int -> Effect Writer
foreign import write_string :: Writer -> String -> Effect Writer
foreign import writer_fork :: Writer -> Effect Writer
foreign import writer_ldelim :: Writer -> Effect Writer
foreign import writer_finish :: Writer -> Uint8Array
