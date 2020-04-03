module DefaultSpec.Common
  ( SimpleT1
  , defaultSimpleT1
  , SimpleT2
  , defaultSimpleT2
  , RecursiveT1(RecursiveT1)
  , defaultRecursiveT1
  , RecursiveT2(RecursiveT2)
  , defaultRecursiveT2 
  ) where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)

type SimpleT1 = { m1 :: Maybe Boolean, b1 :: Boolean, b2 :: Boolean }
defaultSimpleT1 :: { b1 :: Boolean, b2 :: Boolean }
defaultSimpleT1 = { b1: false, b2: true }
type SimpleT2 = { b0 :: Boolean, b1 :: Boolean, b2 :: Boolean }
defaultSimpleT2 :: { b1 :: Boolean, b2 :: Boolean }
defaultSimpleT2 = { b1: false, b2: true }
newtype RecursiveT1 = RecursiveT1 { b1 :: Boolean, b2 :: Boolean, x :: RecursiveT1 }
derive instance eqRecursiveT1 :: Eq RecursiveT1
defaultRecursiveT1 :: { b1 :: Boolean, b2 :: Boolean }
defaultRecursiveT1 = { b1: false, b2: true }
newtype RecursiveT2 = RecursiveT2 { b1 :: Boolean, b2 :: Boolean, x :: Maybe RecursiveT2 }
derive instance eqRecursiveT2 :: Eq RecursiveT2
defaultRecursiveT2 :: { b1 :: Boolean, b2 :: Boolean }
defaultRecursiveT2 = { b1: false, b2: true }