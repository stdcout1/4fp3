module Spec.CharMap (charMapTests) where

import CharMap qualified as CM
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

charMapTests :: TestTree
charMapTests =
  testGroup
    "CharMap"
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
    [ testCase "lookup empty" $
        CM.lookup 'x' (CM.empty :: CM.CharMap Int) @?= Nothing,
      testCase "insert overwrite" $ do
        let m = CM.insert 'a' 2 (CM.insert 'a' 1 CM.empty)
        CM.lookup 'a' m @?= Just (2 :: Int),
      -- YOUR TESTS HERE!
      testCase "empty map is empty" $
        assertBool "isEmpty should be True on empty" (CM.isEmpty (CM.empty :: CM.CharMap Int)),
      testCase "singleton map" $ do
        let m = CM.singleton 'a' (1 :: Int)
        assertBool "singleton is not empty" (not $ CM.isEmpty m)
        CM.isSingleton m @?= Just ('a', 1)
        CM.lookup 'a' m @?= Just 1
        CM.lookup 'b' m @?= Nothing
        CM.toAscList m @?= [('a', 1)],
      testCase "insert overwrite" $ do
        let m = CM.insert 'a' 2 (CM.insert 'a' 1 CM.empty)
        CM.lookup 'a' m @?= Just (2 :: Int),
      testCase "insert and lookup" $ do
        let m =
              CM.insert 'b' 2 $
                CM.insert 'a' 1 $
                  CM.insert 'c' 3 CM.empty
        CM.lookup 'a' m @?= Just (1 :: Int)
        CM.lookup 'b' m @?= Just 2
        CM.lookup 'c' m @?= Just 3
        CM.lookup 'x' m @?= Nothing,
      testCase "insertWith" $ do
        let m1 = CM.insert 'a' 10 CM.empty
            m2 = CM.insertWith (+) 'a' 5 m1
        CM.lookup 'a' m2 @?= Just (15 :: Int),
      testCase "foldlWithKey' is l-r" $ do
        let m = CM.fromList [('d', 4), ('a', 1), ('c', 3), ('b', 2)]
            ks = CM.foldlWithKey' (\acc k _ -> acc ++ [k]) [] m
        ks @?= "abcd"
    ]

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

genKVs :: Gen [(Char, Int)]
genKVs =
  Gen.list (Range.linear 0 1000) $
    (,) <$> Gen.unicode <*> Gen.int (Range.constant (-1000) 1000)

propertyTests :: TestTree
propertyTests =
  testGroup
    "property tests"
    [ testProperty "fromList/insert/lookup behaves identically to Data.Map (Oracle)" prop_oracleMatch,
      testProperty "toAscList behaves identically to Data.Map.toAscList (Oracle)" prop_toAscListOracle,
      testProperty "insertWith behaves like Data.Map.insertWith (Oracle)" prop_insertWithOracle
      -- YOUR TESTS HERE!
    ]

-- | Constructing a CharMap and Data.Map from the same list yields identical
-- lookup results for any key.
prop_oracleMatch :: Property
prop_oracleMatch = property $ do
  kvs <- forAll genKVs

  searchKeys <- forAll $ Gen.list (Range.linear 0 100) Gen.unicode

  let refMap = Map.fromList kvs
      cmMap = foldl (\m (k, v) -> CM.insert k v m) CM.empty kvs
      keysToTest = map fst kvs ++ searchKeys

  traverse_ (\k -> CM.lookup k cmMap === Map.lookup k refMap) keysToTest

prop_toAscListOracle :: Property
prop_toAscListOracle = property $ do
  kvs <- forAll genKVs
  let cmMap = CM.fromList kvs
      refMap = Map.fromList kvs
  CM.toAscList cmMap === Map.toAscList refMap

prop_insertWithOracle :: Property
prop_insertWithOracle = property $ do
  kvs <- forAll genKVs
  k <- forAll Gen.unicode
  v <- forAll $ Gen.int (Range.constant (-1000) 1000)

  let cmMap = CM.fromList kvs
      refMap = Map.fromList kvs

      cmMap' = CM.insertWith (+) k v cmMap
      refMap' = Map.insertWith (+) k v refMap

  CM.toAscList cmMap' === Map.toAscList refMap'
