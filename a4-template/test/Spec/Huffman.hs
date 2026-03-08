module Spec.Huffman (huffmanTests) where

import CharMap qualified as M
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Huffman qualified as H
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

huffmanTests :: TestTree
huffmanTests =
  testGroup
    "Huffman"
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
    [ testCase "frequencies: basic" $
        M.toAscList (H.frequencies "battat") @?= [('a', 2), ('b', 1), ('t', 3)],
      testCase "frequencies: empty" $
        M.toAscList (H.frequencies "") @?= []
      -- YOUR TESTS HERE!
    ]

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

genMsg :: Gen String
genMsg = Gen.string (Range.linear 1 1000) Gen.alphaNum

propertyTests :: TestTree
propertyTests =
  testGroup
    "property tests"
    [ testProperty "frequencies match list-based oracle" prop_frequencies
      -- YOUR TESTS HERE!
    ]

prop_frequencies :: Property
prop_frequencies = property $ do
  s <- forAll genMsg
  -- Hint: You can use Data.List.NonEmpty.group and .sort to compute the oracle
  -- frequencies from the input string.
  _prop_frequencies
