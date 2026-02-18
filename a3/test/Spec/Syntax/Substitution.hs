-- | Tests for 'A3.Syntax.Substitution'.
module Spec.Syntax.Substitution
  ( substitutionSpec
  -- $generators
  , genSubstitution
  ) where

import A3.Syntax.Core
import A3.Syntax.Substitution (Substitution(..), Substitute(..))
import A3.Syntax.Substitution qualified as Sub

import Data.Set qualified as Set

import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Spec.Permutation
import Spec.Syntax.Core

import Test.Tasty
import Test.Tasty.Hedgehog


substitutionSpec :: TestTree
substitutionSpec =
  testGroup "Syntax.Substitution"
  [ testGroup "Substitute instances"
    [ -- Your tests here!
    ]
  ]

-- | Generate a 'Substitution'.
genSubstitution :: forall m. (H.MonadGen m) => m Substitution
genSubstitution = Substitution <$> Gen.map (Range.linear 1 10) ((,) <$> genName <*> genTerm)
