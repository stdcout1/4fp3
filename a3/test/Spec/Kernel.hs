{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
-- | Tests for A3.Kernel
module Spec.Kernel
  ( kernelSpec
  ) where

import Prelude hiding (and, or)

import A3.Kernel
import A3.Pretty (Pretty(..))
import A3.Syntax.Context (Context)
import A3.Syntax.Context qualified as Ctx
import A3.Syntax.Core

import Data.Set qualified as Set

import Test.Tasty
import Test.Tasty.HUnit

-- * Typechecking tests

-- | Assert that a an 'Check' elaborates to a term of a given type.
assertChecks :: Context Type -> Check -> Type -> Term -> Assertion
assertChecks ctx rule tp expectedTm = do
  actualTm <- either (fail . pretty) pure $ runCheck rule ctx tp
  expectedTm @?= actualTm

-- | Assert that a an 'Infer' elaborates to a term of a given type.
assertInfers :: Context Type -> Infer -> Term -> Type -> Assertion
assertInfers ctx rule expectedTm expectedTp = do
  (actualTm, actualTp) <- either (fail . pretty) pure $ runInfer rule ctx
  expectedTm @?= actualTm
  expectedTp @?= actualTp

typecheckSpec :: TestTree
typecheckSpec =
  testGroup "Typechecking"
  [ testCase "var lookup" $
    let (x, ctx) = Ctx.extend Ctx.empty "x" Prop
    in assertInfers ctx (var x) (Var x) Prop
  , testCase "id" $
    let x = freshen "x" Set.empty
    in assertChecks Ctx.empty (lam "x" \x -> chk (var x)) (Fn Prop Prop)
      (Lam (Binder x Prop (Var x)))
  , testCase "const" $
    let x = freshen "x" Set.empty
        y = freshen "y" Set.empty
    in assertChecks Ctx.empty (lam "x" \x -> lam "y" \_ -> chk (var x)) (Fn Prop (Fn Prop Prop))
      (Lam (Binder x Prop (Lam (Binder y Prop (Var x)))))
  -- More tests here!
  ]

-- * Proof tests

-- | Assert that a backwards-mode proof proves a goal.
assertBackward :: Context Type -> Context Term -> Check -> Backward -> Assertion
assertBackward ctx hyps goalTac proofTac = do
  goal <- either (fail . pretty) pure $ runCheck goalTac ctx Prop
  either (fail . pretty) pure $ runBackward proofTac ctx hyps goal

symSpec :: TestTree
symSpec =
  testCase "sym" $ assertBackward Ctx.empty Ctx.empty goal proof
  where
    goal =
      forAll "x" Prop \x ->
      forAll "y" Prop \y ->
      eq (var x) (chk $ var y) `implies` eq (var y) (chk $ var x)

    proof =
      forAllIntro "x" \_ ->
      forAllIntro "y" \y ->
      impliesIntro "p" \p ->
      forward $
      leibniz
        (assumption p)
        "x" (\x -> eq (var y) (chk $ var x))
        refl

transSpec :: TestTree
transSpec =
  testCase "trans" $ assertBackward Ctx.empty Ctx.empty goal proof
  where
    goal =
      forAll "x" Prop \x ->
      forAll "y" Prop \y ->
      forAll "z" Prop \z ->
      (eq (var x) (chk $ var y) `and` eq (var y) (chk $ var z)) `implies` eq (var x) (chk $ var z)

    proof =
      forAllIntro "x" \x ->
      forAllIntro "y" \y ->
      forAllIntro "z" \z ->
      impliesIntro "pq" \pq ->
      forward $
      leibniz
        (andElimLeft (assumption pq))
        "x" (\x -> eq (var x) (chk $ var z))
        (forward $ andElimRight (assumption pq))


proofSpec :: TestTree
proofSpec =
  testGroup "Proofs"
  [ symSpec
  , transSpec
  -- More tests here!
  ]

kernelSpec :: TestTree
kernelSpec =
  testGroup "Kernel"
  [ typecheckSpec
  , proofSpec
  ]
