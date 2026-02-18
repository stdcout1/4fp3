-- | Tests for 'A3.Permutation'
module Spec.Permutation
  ( permutationSpec
    -- $generators
  , genDisjointPermutation
  , genPermutation
  ) where

import A3.Permutation (Permutation)
import A3.Permutation qualified as P

import Data.Set (Set)
import Data.Set qualified as Set

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

permutationSpec :: TestTree
permutationSpec =
  testGroup "Permutation"
  [ testGroup "permute"
    [ testProperty "permute identity" $ property do
        x <- forAll $ Gen.int (Range.linear 0 100)
        P.permute x mempty === x
      -- Your tests for permute x (p1 <> p2) == permute (permute x p1) p2 here

      -- ∀(x : Int). ∀(p1, p2 : Permutation Int). permute x (p1 <> p2) = permute (permute x p1) p2
      ,testProperty "permute 2x" $ property do 
        x <- forAll $ Gen.int (Range.linear 0 100)
        p1 <- forAll $ genPermutation $ Gen.int $ Range.linear 0 100
        p2 <- forAll $ genPermutation $ Gen.int $ Range.linear 0 100
        P.permute x (p1 <> p2) === P.permute (P.permute x p1) p2
    ]
  , testGroup "compose"
    [ -- Your tests for associativity/unitality of permutation composition here.
        -- ∀(p1, p2, p3 : Permutation Int). p1 <> (p2 <> p3) = (p1 <> p2) <> p3
        testProperty "associativity" $ property do 
            p1 <- forAll $ genPermutation $ Gen.int $ Range.linear 0 100
            p2 <- forAll $ genPermutation $ Gen.int $ Range.linear 0 100
            p3 <- forAll $ genPermutation $ Gen.int $ Range.linear 0 100
            p1 <> (p2 <> p3) === (p1 <> p2) <> p3 
        ,testProperty "unitality right" $ property do 
            p <- forAll $ genPermutation $ Gen.int $ Range.linear 0 100
            p <> mempty === p 
        ,testProperty "unitality left" $ property do 
            p <- forAll $ genPermutation $ Gen.int $ Range.linear 0 100
            mempty <> p === p 
    ]
  ]

-- $generators
-- * Generators

-- | Generate a genPermutation that avoids permuting a set of elements.
genDisjointPermutation :: forall m a. (MonadGen m, Ord a) => Set a -> m a -> m (Permutation a)
genDisjointPermutation avoid gen =
  -- Basic algorith here is to generate a random set of numbers,
  -- remove all the elements are are trying to avoid, and then
  -- randomly shuffle the set as a list
  --
  -- Then, we break that list up into runs of random length. This
  -- gives us a bunch of cycles, which we can use to build a permutation.
  Gen.shrink shrink do
    xs <- Gen.set (Range.linear 0 100) gen
    let disjoint = Set.difference xs avoid
    shuffled <- Gen.shuffle $ Set.toList disjoint
    -- unsafeFromCycles is safe here, as we built the cycle list from
    -- a set.
    P.unsafeFromCycles <$> partitionList (Set.size disjoint) shuffled
  where
    partitionList :: Int -> [a] -> m [[a]]
    partitionList 0 _xs = pure []
    partitionList n xs = do
      k <- Gen.int (Range.constant 1 n)
      let (chunk, rest) = splitAt k xs
      (chunk:) <$> partitionList (n - k) rest

    shrink :: Permutation a -> [Permutation a]
    shrink p = [ P.deleteCycle x p | x <- Set.toList (P.support p) ]

-- | Generate a permutation.
genPermutation :: forall m a. (MonadGen m, Ord a) => m a -> m (Permutation a)
genPermutation = genDisjointPermutation Set.empty
