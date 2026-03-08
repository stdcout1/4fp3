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
        CM.lookup 'a' m @?= Just (2 :: Int)
      -- YOUR TESTS HERE!
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
    [ testProperty "fromList/insert/lookup behaves identically to Data.Map (Oracle)" prop_oracleMatch
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
