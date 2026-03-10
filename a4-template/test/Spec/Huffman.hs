module Spec.Huffman (huffmanTests) where

import CharMap qualified as M
import Data.List (sort)
import Data.List.NonEmpty (group)
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
        M.toAscList (H.frequencies "") @?= [],
      testCase "encode/decode with a long word" $ do
        let msg = "pneumonoultramicroscopicsilicovolcanoconiosis"
        case H.buildTree msg of
          Nothing ->
            assertFailure "no tree :("
          Just tr -> do
            let tbl = H.codeTable tr
                bits = H.encodeMessage tbl msg
            H.decodeMessage tr bits @?= msg
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
    [ testProperty "frequencies match list-based oracle" prop_frequencies,
      testProperty "table keys equal frequency keys" prop_codeTable_keys,
      testProperty "encode/decode are inverses" prop_decodeRemovesEncode
    ]

prop_frequencies :: Property
prop_frequencies = property $ do
  s <- forAll genMsg
  -- Hint: You can use Data.List.NonEmpty.group and .sort to compute the oracle
  -- frequencies from the input string.
  M.toAscList (H.frequencies s) === oracleFreq s
  where
    -- group things by thier (c, n) just like H.freq then sort in asc...
    oracleFreq = map (\g -> (NE.head g, length g)) . NE.group . sort

prop_codeTable_keys :: Property
prop_codeTable_keys = property $ do
  s <- forAll genMsg
  case H.buildTree s of
    Nothing ->
      failure
    Just tr ->
        -- remeber we added \0 so remove it
      realTableKeys tr === map fst (M.toAscList (H.frequencies s))
  where
    realTableKeys :: H.Tree -> [Char]
    realTableKeys =
      filter (/= '\0') . map fst . M.toAscList . H.codeTable


prop_decodeRemovesEncode :: Property
prop_decodeRemovesEncode = property $ do
  s <- forAll genMsg
  case H.buildTree s of
    Nothing ->
      failure
    Just tr -> do
      let tbl = H.codeTable tr
          bits = H.encodeMessage tbl s
      H.decodeMessage tr bits === s
