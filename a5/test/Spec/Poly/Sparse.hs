{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Specification for @A5.Poly.Sparse@.
module Spec.Poly.Sparse
  ( sparsePolySpec
  ) where

import A5.Language.Ring (Ring, Monus, One)
import A5.Language.Ring qualified as R

import A5.Poly.Sparse (Poly)
import A5.Poly.Sparse qualified as Poly

import Data.Semigroup

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

ringSpec
  :: (Show r, Ord r, Ring r, Monus p, One p, Ord p, Show p, Ord x, Show x)
  => Gen (Poly r p x)
  -> [TestTree]
ringSpec genPoly =
  [ testProperty "p + 0 = p" $ property do
      p <- forAll genPoly
      p Poly.+ Poly.const (R.int 0) === p
  , testProperty "0 + p = p" $ property do
      p <- forAll genPoly
      Poly.const (R.int 0) Poly.+ p === p
  , testProperty "p + q = q + p" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      p Poly.+ q === q Poly.+ p
  , testProperty "p + (q + r) = (p + q) + r" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      r <- forAll genPoly
      p Poly.+ (q Poly.+ r) === (p Poly.+ q) Poly.+ r
  , testProperty "p * 0 = 0" $ property do
      p <- forAll genPoly
      p Poly.* Poly.const (R.int 0) === Poly.const (R.int 0)
  , testProperty "0 * p = 0" $ property do
      p <- forAll genPoly
      Poly.const (R.int 0) Poly.* p === Poly.const (R.int 0)
  , testProperty "p * 1 = p" $ property do
      p <- forAll genPoly
      p Poly.* Poly.const (R.int 1) === p
  , testProperty "1 * p = p" $ property do
      p <- forAll genPoly
      Poly.const (R.int 1) Poly.* p === p
  , testProperty "p * q = q * p" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      p Poly.* q === q Poly.* p
  , testProperty "p * (q * r) = (p * q) * r" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      r <- forAll genPoly
      p Poly.* (q Poly.* r) === (p Poly.* q) Poly.* r
  , testProperty "p * (q + r) = p * q + p * r" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      r <- forAll genPoly
      p Poly.* (q Poly.+ r) === (p Poly.* q) Poly.+ (p Poly.* r)
  , testProperty "(p + q) * r = p * r + q * r" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      r <- forAll genPoly
      (p Poly.+ q) Poly.* r === (p Poly.* r) Poly.+ (q Poly.* r)
  ]

evalHomomorphicSpec
  :: (Show r, Ord r, Ring r, Monus p, One p, Ord p, Show p)
  => Gen (Poly r p r)
  -> (r -> p -> r)
  -> [TestTree]
evalHomomorphicSpec genPoly powCoeff =
  let evalPoly p = Poly.eval p id powCoeff id
  in
  [ testProperty "eval (p + q) (^) id = eval p (^) id + eval p (^) id" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      evalPoly (p Poly.+ q) === (evalPoly p R.+ evalPoly q)
  , testProperty "eval (p * q) (^) id = eval p (^) id * eval p (^) id" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      evalPoly (p Poly.* q) === (evalPoly p R.* evalPoly q)
  , testProperty "eval (p - q) (^) id = eval p (^) id - eval p (^) id" $ property do
      p <- forAll genPoly
      q <- forAll genPoly
      evalPoly (p Poly.- q) === (evalPoly p R.- evalPoly q)
  , testProperty "eval (negate p) (^) id = negate (eval p (^) id)" $ property do
      p <- forAll genPoly
      evalPoly (Poly.negate p) === R.negate (evalPoly p)
  , testProperty "eval (p ^ n) (^) id = (eval p (^) id) ^ n" $ property do
      p <- forAll genPoly
      n <- forAll $ Gen.word (Range.linear 0 10)
      evalPoly (p Poly.^ n) === (evalPoly p R.^ n)
  ]

sparsePolySpec :: TestTree
sparsePolySpec =
  testGroup "Poly.Sparse"
  [ testGroup "valid"
    [ testProperty "Poly Int (Sum Word) Char" $ property do
      p <- forAll $ genPoly (Gen.int $ Range.linearFrom 0 (-10) 10) (Sum <$> Gen.word (Range.linear 0 10)) Gen.lower
      assert (Poly.valid p)
    ]
  , testGroup "Ring"
    [ testGroup "Poly Int (Sum Word) Int" $
      let genCoeff = Gen.int $ Range.linearFrom 0 (-10) 10
          genPow = Sum <$> Gen.word (Range.linear 0 10)
          genVar = Gen.lower
      in ringSpec (genPoly genCoeff genPow genVar)
    ]
  , testGroup "eval"
    [ testGroup "Poly Int (Sum Word) Int" $
      let genCoeff = Gen.int $ Range.linearFrom 0 (-10) 10
          genPow = Sum <$> Gen.word (Range.linear 0 10)
          powCoeff x n = x R.^ getSum n
      in evalHomomorphicSpec (genPoly genCoeff genPow genCoeff) powCoeff
    ]
  ]

--------------------------------------------------------------------------------
-- Generators

genPoly :: (MonadGen m, Ring r, Eq r, Monus p, Ord p, One p, Ord x) => m r -> m p -> m x -> m (Poly r p x)
genPoly genr genp genx =
  Gen.recursive Gen.choice
  [ Poly.const <$> genr
  , Poly.monomial <$> genx <*> genp
  ]
  [ Gen.subterm2 (genPoly genr genp genx) (genPoly genr genp genx) (Poly.+)
  , Gen.subterm2 (genPoly genr genp genx) (genPoly genr genp genx) (Poly.*)
  , Gen.subterm2 (genPoly genr genp genx) (genPoly genr genp genx) (Poly.-)
  , Gen.subterm (genPoly genr genp genx) Poly.negate
  , Gen.subtermM (genPoly genr genp genx) (\p -> (p Poly.^) <$> Gen.word (Range.linear 0 10))
  ]
