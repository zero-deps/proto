module Cases where

import Prelude (negate)
import SchemaCommon
import Data.Tuple (Tuple(Tuple))
import Proto.BigInt as BigInt

map_schema :: TestSchema
map_schema = ClassWithMap { m: [ Tuple "en_GB" "Hello", Tuple "it_IT" "Ciao" ] }

map_bytestr :: String
map_bytestr = "10 31 10 14 10 5 101 110 95 71 66 18 5 72 101 108 108 111 10 13 10 5 105 116 95 73 84 18 4 67 105 97 111"

maxlong_schema :: TestSchema
maxlong_schema = ClassWithLong { x: BigInt.fromNumber (9007199254740991.0) }

maxlong_bytestr :: String
maxlong_bytestr = "18 9 8 255 255 255 255 255 255 255 15"

minlong_schema :: TestSchema
minlong_schema = ClassWithLong { x: BigInt.fromNumber (-9007199254740991.0) }

minlong_bytestr :: String
minlong_bytestr = "18 11 8 129 128 128 128 128 128 128 240 255 1"

max_bigint_schema :: TestSchema
max_bigint_schema = ClassWithLong { x: BigInt.fromString "9223372036854775807" }

max_bigint_bytestr :: String
max_bigint_bytestr = "18 10 8 255 255 255 255 255 255 255 255 127"

min_bigint_schema :: TestSchema
min_bigint_schema = ClassWithLong { x: BigInt.fromString "-9223372036854775808" }

min_bigint_bytestr :: String
min_bigint_bytestr = "18 11 8 128 128 128 128 128 128 128 128 128 1"

maxint_schema :: TestSchema
maxint_schema = ClassWithInt { x: 2147483647 }

maxint_bytestr :: String
maxint_bytestr = "26 6 8 255 255 255 255 7"

minint_schema :: TestSchema
minint_schema = ClassWithInt { x: -2147483648 }

minint_bytestr :: String
minint_bytestr = "26 11 8 128 128 128 128 248 255 255 255 255 1"
