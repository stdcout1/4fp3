-- | Tests for 'A3.Syntax.Substitution'.
module Spec.Syntax.Substitution
  ( substitutionSpec
  -- $generators
  , genSubstitution
  ) where

import A3.Syntax.Core
import A3.Syntax.Substitution (Substitution(..), Substitute(..), terms, restrict, vars)
import A3.Syntax.Substitution qualified as Sub

import Data.Set qualified as Set

import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Spec.Permutation
import Spec.Syntax.Core

import Test.Tasty
import Test.Tasty.Hedgehog

lawfulSubstitionSpec :: (Show a, Eq a, Rename a, Substitute a) => H.Gen a -> [TestTree]
lawfulSubstitionSpec gen = 
    -- substs a mempty == a
    [ testProperty "substitution identity" $ H.property do 
        a <- H.forAll gen 
        substs a mempty H.=== a

    -- substs a (sub1 <> sub2) == substs (substs a sub1) sub2
    , testProperty "substitution compose" $ H.property do 
        a <- H.forAll gen
        s1 <- H.forAll genSubstitution
        s2 <- H.forAll genSubstitution
        substs a (s1 <> s2) H.=== substs (substs a s1) s2

    -- rename (substs a sub) p == substs (rename a p) (rename sub p)
    , testProperty "renaming a substitution" $ H.property do 
        a <- H.forAll gen 
        s <- H.forAll genSubstitution
        p <- H.forAll (genPermutation genName)
        rename (substs a s) p H.=== substs (rename a p) (rename s p)

    -- freeVars (substs a sub) == Set.union (freeVars (terms $ restrict (freeVars a) sub)) (Set.difference (freeVars a) (vars sub))
    , testProperty "free variables of a substitution" $ H.property do 
        a <- H.forAll gen 
        s <- H.forAll genSubstitution
        freeVars (substs a s) H.=== Set.union (freeVars (terms $ restrict (freeVars a) s)) (Set.difference (freeVars a) (vars s))
    ]

substitutionSpec :: TestTree
substitutionSpec =
  testGroup "Syntax.Substitution"
  [ testGroup "Substitute instances"
    [ testGroup "(Substitute, Substitute)" $ lawfulSubstitionSpec ((,) <$> genTerm <*> genTerm)
    , testGroup "Binder" $ lawfulSubstitionSpec genTerm
    , testGroup "Term" $ lawfulSubstitionSpec genTerm
    ]
  ]

-- | Generate a 'Substitution'.
genSubstitution :: forall m. (H.MonadGen m) => m Substitution
genSubstitution = Substitution <$> Gen.map (Range.linear 1 10) ((,) <$> genName <*> genTerm)
