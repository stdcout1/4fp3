module Spec.Para (paraSpec) where

import A6.CataAna (List, fromList, toList)
import A6.Para (deleteMin, findMin, wordCount)
import Data.Maybe (fromJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testList :: List Int
testList = fromList [1, 2, 1, 3]

paraSpec :: TestTree
paraSpec =
  testGroup
    "Para"
    [ -- FIXME: add tests!
      testCase "min of [1,2,1,3] is 1, [2,1,3]" $
        (fmap toList (fromJust (deleteMin testList))) @?= (1, [2, 1, 3]),
      testCase "deleteMin of [5] returns (5, [])" $
        fmap toList (fromJust (deleteMin (fromList [5]))) @?= (5, []),
      testCase "deleteMin of [] returns Nothing" $
        isNothing (deleteMin (fromList ([] :: [Int]))) @?= True,
      testCase "deleteMin of [2,2,2] returns (2, [2,2])" $
        fmap toList (fromJust (deleteMin (fromList [2, 2, 2]))) @?= (2, [2, 2]),
      testCase "deleteMin of [4,3,2,1] returns (1, [4,3,2])" $
        fmap toList (fromJust (deleteMin (fromList [4, 3, 2, 1]))) @?= (1, [4, 3, 2]),
      testCase "deleteMin of [1,2,1,3] returns (1, [2,1,3])" $
        fmap toList (fromJust (deleteMin (fromList [1, 2, 1, 3]))) @?= (1, [2, 1, 3]),
      testCase "deleteMin of [3,1,4,1,5] returns (1, [3,4,1,5])" $
        fmap toList (fromJust (deleteMin (fromList [3, 1, 4, 1, 5]))) @?= (1, [3, 4, 1, 5]),
      testCase "findMin of [] returns Nothing" $
        isNothing (findMin (fromList ([] :: [Int]))) @?= True,
      testCase "findMin of [5] returns ([], 5, [])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [5]))) @?= ([], 5, []),
      testCase "findMin of [1,2,3] returns ([], 1, [2,3])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [1, 2, 3]))) @?= ([], 1, [2, 3]),
      testCase "findMin of [3,1,2] returns ([3], 1, [2])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [3, 1, 2]))) @?= ([3], 1, [2]),
      testCase "findMin of [4,3,2,1] returns ([4,3,2], 1, [])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [4, 3, 2, 1]))) @?= ([4, 3, 2], 1, []),
      testCase "findMin of [2,2,2] returns ([], 2, [2,2])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [2, 2, 2]))) @?= ([], 2, [2, 2]),
      testCase "findMin of [3,1,4,1,5] returns ([3], 1, [4,1,5])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [3, 1, 4, 1, 5]))) @?= ([3], 1, [4, 1, 5]),
      testCase "findMin of [3,1,2,1] returns ([3], 1, [2,1])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [3, 1, 2, 1]))) @?= ([3], 1, [2, 1]),
      testCase "findMin of [1,2,1,3] returns ([], 1, [2,1,3])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [1, 2, 1, 3]))) @?= ([], 1, [2, 1, 3]),
      testCase "findMin of [2,3,1,4] returns ([2,3], 1, [4])" $
        (\(b, m, a) -> (toList b, m, toList a)) (fromJust (findMin (fromList [2, 3, 1, 4]))) @?= ([2, 3], 1, [4])
      ,testCase "Word count of ' Hello World ' is 2" $
        wordCount (fromList " Hello World ") @?= 2
    ]
