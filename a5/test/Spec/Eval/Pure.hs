-- | Spec for @A5.Eval.Pure@.
module Spec.Eval.Pure
  ( evalPureSpec
  ) where

import A5.Data.Array.Pure qualified as Array
import A5.Eval.Pure

import Spec.Programs

import Test.Tasty
import Test.Tasty.HUnit

assertComputesTo :: ComputeM Val -> Val -> Assertion
assertComputesTo c expected = do
  actual <- either fail pure $ runComputeM c []
  actual @?= expected

assertCrashesWith :: ComputeM Val -> String -> Assertion
assertCrashesWith c msg =
  let actual = runComputeM c []
  in actual @?= Left msg

evalPureSpec :: TestTree
evalPureSpec =
  testGroup "Eval.Pure"
  [ testCase "plus 2 5 == 7" $
      assertComputesTo (plus (VInt 2) (VInt 5)) (VInt 7)
  , testCase "times 2 5 == 10" $
      assertComputesTo (times (VInt 2) (VInt 5)) (VInt 10)
  , testCase "insertionSort [2,4,8,1,3,1] == [1,1,2,3,4,8]" $ do
    let array  = VArray $ Array.fromList [VInt 2, VInt 4, VInt 8, VInt 1, VInt 3, VInt 1]
        sorted = VArray $ Array.fromList [VInt 1, VInt 1, VInt 2, VInt 3, VInt 4, VInt 8]
    assertComputesTo (insertionSort array) sorted
  , testCase "alwaysCrash true" $ do
      assertCrashesWith (alwaysCrash (VBool True)) $ unlines
        [ "Type error: expected an int but got a bool"
        , ""
        , "Backtrace:"
        , "add"
        , "if"
        ]
  ]
