module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Spec.CataAna (cataAnaSpec)
import Spec.Para (paraSpec)
import Spec.RegExp (regExpSpec)

main :: IO ()
main = defaultMain $
  testGroup "A6"
  [ cataAnaSpec
  , paraSpec
  , regExpSpec
  ]
