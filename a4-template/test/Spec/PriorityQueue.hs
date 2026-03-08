module Spec.PriorityQueue (priorityQueueTests) where

import Data.List (sort)
import Data.Maybe (isNothing)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PriorityQueue (toPrioList)
import PriorityQueue qualified as PQ
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

priorityQueueTests :: TestTree
priorityQueueTests =
  testGroup
    "PriorityQueue"
    [ unitTests,
      propertyTests
    ]

--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests"
    [ testCase "empty heap" $ do
        assertBool "isEmpty is True" (PQ.isEmpty (PQ.empty @Int))
        assertBool "findMin is Nothing" (isNothing $ PQ.findMin (PQ.empty @Int))
        assertBool "deleteMin is Nothing" (isNothing $ PQ.deleteMin (PQ.empty @Int)),
      testCase "singleton heap" $ do
        let h = PQ.singleton 42
        assertBool "singleton is not empty" (not $ PQ.isEmpty h)
        PQ.isSingleton h @?= Just 42
        PQ.findMin h @?= Just 42
        PQ.toPrioList h @?= [42],
      testCase "isSingleton is Nothing for empty heap" $ do
        PQ.isSingleton (PQ.empty @Int) @?= Nothing,
      testCase "isSingleton is Nothing for non-singleton heap" $ do
        let h = PQ.insert 1 (PQ.singleton 2)
        PQ.isSingleton h @?= Nothing,
      testCase "insert into empty heap" $ do
        let h = PQ.insert 7 PQ.empty
        PQ.findMin h @?= Just 7
        PQ.toPrioList h @?= [7],
      testCase "insert smaller element updates minimum" $ do
        let h = PQ.insert 10 (PQ.insert 20 PQ.empty)
        let h' = PQ.insert 5 h
        PQ.findMin h' @?= Just 5
        PQ.toPrioList h' @?= [5, 10, 20],
      testCase "merge with empty on left" $ do
        let h = PQ.fromList [3, 1, 2]
        PQ.merge PQ.empty h @?= h,
      testCase "merge with empty on right" $ do
        let h = PQ.fromList [3, 1, 2]
        PQ.merge h PQ.empty @?= h,
      testCase "merge combines heaps correctly" $ do
        let h1 = PQ.fromList [5, 1, 9]
        let h2 = PQ.fromList [4, 2, 8]
        PQ.toPrioList (PQ.merge h1 h2) @?= [1, 2, 4, 5, 8, 9],
      testCase "findMin from fromList" $ do
        let h = PQ.fromList [9, 4, 7, 1, 3]
        PQ.findMin h @?= Just 1,
      testCase "deleteMin removes only the minimum" $ do
        let h = PQ.fromList [5, 1, 3, 2, 4]
        case PQ.deleteMin h of
          Nothing -> assertFailure "deleteMin returned Nothing"
          Just h' -> PQ.toPrioList h' @?= [2, 3, 4, 5],
      testCase "deleteMin on singleton gives empty heap" $ do
        case PQ.deleteMin (PQ.singleton 99) of
          Nothing -> assertFailure "deleteMin returned Nothing"
          Just h' -> assertBool "heap should be empty" (PQ.isEmpty h'),
      testCase "popMin on empty heap is Nothing" $ do
        PQ.popMin (PQ.empty @Int) @?= Nothing,
      testCase "popMin on singleton heap" $ do
        case PQ.popMin (PQ.singleton 11) of
          Nothing -> assertFailure "popMin returned Nothing"
          Just (x, h') -> do
            x @?= 11
            assertBool "remaining heap should be empty" (PQ.isEmpty h'),
      testCase "popMin returns min and remainder" $ do
        let h = PQ.fromList [4, 1, 3, 2]
        case PQ.popMin h of
          Nothing -> assertFailure "popMin returned Nothing"
          Just (x, h') -> do
            x @?= 1
            PQ.toPrioList h' @?= [2, 3, 4],
      testCase "duplicates are retained" $ do
        let h = PQ.fromList [3, 1, 2, 1, 3, 2, 1]
        PQ.toPrioList h @?= [1, 1, 1, 2, 2, 3, 3],
      testCase "fromList/toPrioList agrees with sort" $ do
        let xs = [7, 3, 9, 1, 4, 1, 8, 2]
        PQ.toPrioList (PQ.fromList xs) @?= sort xs
    ]

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

genInts :: Gen [Int]
genInts = Gen.list (Range.linear 0 1000) (Gen.int (Range.constant (-10000) 10000))

propertyTests :: TestTree
propertyTests =
  testGroup
    "property tests"
    [ testProperty "fromList then toPrioList equals sort" $ property $ do
        xs <- forAll genInts
        PQ.toPrioList (PQ.fromList xs) === sort xs

    , testProperty "merge agrees with sorting concatenated lists" $ property $ do
        xs <- forAll genInts
        ys <- forAll genInts
        let h1 = PQ.fromList xs
        let h2 = PQ.fromList ys
        PQ.toPrioList (PQ.merge h1 h2) === sort (xs ++ ys)

    , testProperty "findMin matches minimum of sorted list" $ property $ do
        xs <- forAll $ Gen.list (Range.linear 1 1000) (Gen.int (Range.constant (-10000) 10000))
        let h = PQ.fromList xs
        PQ.findMin h === Just (head (sort xs))
    -- maybe check tail == snd $ popmin ?
    ]

-- YOUR TESTS HERE!
