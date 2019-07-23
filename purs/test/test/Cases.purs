module Cases where

import SchemaCommon
import Data.Map as Map
import Data.Tuple (Tuple(Tuple))

c1 :: TestSchema
c1 = ClassWithMap { m: Map.fromFoldable [ Tuple "en_GB" "Name", Tuple "ro_RO" "Nome" ] }

r1 :: String
r1 = "10 30 10 13 10 5 101 110 95 71 66 18 4 78 97 109 101 10 13 10 5 114 111 95 82 79 18 4 78 111 109 101"