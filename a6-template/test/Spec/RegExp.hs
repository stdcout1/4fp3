module Spec.RegExp (regExpSpec) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import A6.RegExp

regExpSpec :: TestTree
regExpSpec =
  testGroup
    "RegExp"
    [ testCase "Eps one empty string" $
        powerSeries 5 Eps @?= [1, 0, 0, 0, 0],
      testCase "Ch 'a' one string of length 1" $
        powerSeries 5 (Ch 'a') @?= [0, 1, 0, 0, 0],
      testCase "a|b two strings of length 1" $
        powerSeries 5 (Ch 'a' :+: Ch 'b') @?= [0, 2, 0, 0, 0],
      testCase "ab one string of length 2" $
        powerSeries 5 (Ch 'a' :->: Ch 'b') @?= [0, 0, 1, 0, 0],
      testCase "a* one string of every length" $
        powerSeries 5 (Star (Ch 'a')) @?= [1, 1, 1, 1, 1],
      testCase "(a|b)* 1,2,4,8,16 strings" $
        powerSeries 5 (Star (Ch 'a' :+: Ch 'b')) @?= [1, 2, 4, 8, 16]
    ]
