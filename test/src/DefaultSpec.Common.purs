module DefaultSpec.Common
  ( SimpleT1
  , defaultSimpleT1
  , SimpleT2
  , defaultSimpleT2
  , RecursiveT(RecursiveT)
  , defaultRecursiveT 
  ) where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)

type SimpleT1 = { m1 :: Maybe Boolean, b1 :: Boolean, b2 :: Boolean }
defaultSimpleT1 :: { b1 :: Boolean, b2 :: Boolean }
defaultSimpleT1 = { b1: false, b2: true }
type SimpleT2 = { b0 :: Boolean, b1 :: Boolean, b2 :: Boolean }
defaultSimpleT2 :: { b1 :: Boolean, b2 :: Boolean }
defaultSimpleT2 = { b1: false, b2: true }
newtype RecursiveT = RecursiveT { b1 :: Boolean, b2 :: Boolean, x :: RecursiveT }
defaultRecursiveT :: { b1 :: Boolean, b2 :: Boolean }
defaultRecursiveT = { b1: false, b2: true }
derive instance eqRecursiveT :: Eq RecursiveT