module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import T01

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tutorial 1 Tests"
  [ testGroup "triple"
      [ testCase "Triple positive" $ triple 3 @?= 9
      , testCase "Triple negative" $ triple (-2) @?= -6
      , testCase "Triple zero"     $ triple 0 @?= 0
      ]
  , testGroup "isOdd"
      [ testCase "Odd number"  $ isOdd 3 @?= True
      , testCase "Even number" $ isOdd 4 @?= False
      ]
  , testGroup "average"
      [ testCase "Clean division" $ average [10, 20, 30] @?= 20
      , testCase "Floor division" $ average [10, 10, 11] @?= 10
      ]
  , testGroup "invert"
      [ testCase "True to False" $ invert True @?= False
      , testCase "False to True" $ invert False @?= True
      ]
  ]
