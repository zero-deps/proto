module DefaultSpec.Common
  ( SimpleT1
  , defaultSimpleT1
  , SimpleT2
  , defaultSimpleT2
  , RecursiveT1(RecursiveT1)
  , defaultRecursiveT1
  , RecursiveT2(RecursiveT2)
  , defaultRecursiveT2
  , OneMaybe
  , defaultOneMaybe
  , OneSeq
  , defaultOneSeq 
  ) where

import Data.Eq (class Eq)
import Data.Maybe (Maybe(Nothing))

type SimpleT1 = { m1 :: Maybe Boolean, b1 :: Boolean, b2 :: String }
defaultSimpleT1 :: { m1 :: Maybe Boolean, b1 :: Boolean, b2 :: String }
defaultSimpleT1 = { m1: Nothing, b1: false, b2: "" }
type SimpleT2 = { b0 :: Boolean, b1 :: Boolean, b2 :: String }
defaultSimpleT2 :: { b1 :: Boolean, b2 :: String }
defaultSimpleT2 = { b1: false, b2: "" }
newtype RecursiveT1 = RecursiveT1 { b1 :: Boolean, b2 :: String, x :: RecursiveT1 }
derive instance eqRecursiveT1 :: Eq RecursiveT1
defaultRecursiveT1 :: { b1 :: Boolean, b2 :: String }
defaultRecursiveT1 = { b1: false, b2: "" }
newtype RecursiveT2 = RecursiveT2 { b1 :: Boolean, b2 :: String, x :: Maybe RecursiveT2 }
derive instance eqRecursiveT2 :: Eq RecursiveT2
defaultRecursiveT2 :: { b1 :: Boolean, b2 :: String, x :: Maybe RecursiveT2 }
defaultRecursiveT2 = { b1: false, b2: "", x: Nothing }
type OneMaybe = { m1 :: Maybe String }
defaultOneMaybe :: { m1 :: Maybe String }
defaultOneMaybe = { m1: Nothing }
type OneSeq = { xs :: Array String }
defaultOneSeq :: { xs :: Array String }
defaultOneSeq = { xs: [] }