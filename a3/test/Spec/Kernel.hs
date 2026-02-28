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
  , testCase "exists" $ 
    let x = freshen "x" Set.empty
        y = freshen "y" Set.empty
    in assertChecks Ctx.empty (exists "x" Prop (\x -> exists "y" Prop \_ -> chk (var x) ) ) Prop (Exists (Binder x Prop (Exists (Binder y Prop (Var x)))))
  , testCase "random prop statment should be prop" $
      let example = And Top Bot `Implies` Or Top Bot
      in assertChecks Ctx.empty
        (implies (and top bot) (or top bot))
        Prop
        example
  , testCase "eq should check if they are the same type" $ 
    let (x, ctx) = Ctx.extend Ctx.empty "x" Prop 
        example = Eq (Var x) (Var x) Prop 
    in assertChecks ctx 
                    -- we need to check this
        (eq (var x) (chk $ var x))
        Prop 
        example
  , testCase "app mismatch (app requires function type)" $
    let (x, ctx) = Ctx.extend Ctx.empty "x" Prop
    in case runInfer (app (var x) top) ctx of
        Left (InferMismatch _ _ _ _ _) -> pure ()
        Left e -> fail ("Expected InferMismatch, got: " <> pretty e)
        Right (tm, tp) ->
            fail ("Expected failure, but got: " <> show tm <> " : " <> show tp)
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

-- Bot + impliesIntro + botElim
exFalsoSpec :: TestTree
exFalsoSpec =
  testCase "ex-falso" $ assertBackward Ctx.empty Ctx.empty goal proof
  where
    goal =
      forAll "p" Prop \p ->
      bot `implies` (chk $ var $ p)

    proof =
      forAllIntro "p" \_ ->
      impliesIntro "b" \b ->
      botElim (forward $ assumption b)

-- (p v p) -> p
-- orElim + impliesIntro 
orElimIdemSpec :: TestTree
orElimIdemSpec =
  testCase "or-elim-idem" $ assertBackward Ctx.empty Ctx.empty goal proof
  where
    goal =
      forAll "p" Prop \p ->
      ((chk $ var p) `or` (chk $ var p)) `implies` (chk $ var p)

    proof =
      forAllIntro "p" \p ->
      impliesIntro "pp" \pp ->
      forward $
      orElim
        (assumption pp)        -- check each one 
        "p1" (\p1 -> assumption p1)  
        "p2" (\p2 -> forward (assumption p2)) 


-- exists intro + exists elim + impliesIntro + forallIntro 
-- exists x -> exists x 
existsIntroSpec :: TestTree
existsIntroSpec =
  testCase "exists-intro" $ assertBackward Ctx.empty Ctx.empty goal proof
  where
    goal =
      forAll "p" Prop \p ->
      chk (var p) `implies` exists "x" Prop (\x -> chk $ var x)

    proof =
      forAllIntro "p" \p ->
      impliesIntro "hp" \hp ->
      existsIntro (chk $ var p) $
        forward (assumption hp)
-- andIntro + andElimLeft + andElimRight + impliesIntro + forallIntro
-- forall (p ^ q) -> (q ^ p)
andCommSpec :: TestTree
andCommSpec =
  testCase "and-comm" $ assertBackward Ctx.empty Ctx.empty goal proof
  where
    goal =
      forAll "p" Prop \p ->
      forAll "q" Prop \q ->
      ((chk $ var p) `and` (chk $ var q)) `implies` ((chk $ var q) `and` (chk $ var p))

    proof =
      forAllIntro "p" \p ->
      forAllIntro "q" \q ->
      impliesIntro "pq" \pq ->
      andIntro
        (forward $ andElimRight (assumption pq))
        (forward $ andElimLeft  (assumption pq))



-- forallElim + impliesElim + impliesIntro
--  forall x -> x -> bot -> bot)
forallElimSpec :: TestTree
forallElimSpec =
  testCase "forall-elim" $ assertBackward Ctx.empty Ctx.empty goal proof
  where
    goal =
      (forAll "x" Prop (\x -> (chk $ var x) `implies` (chk $ var x)))
      `implies`
      (bot `implies` bot)

    proof =
      impliesIntro "f" \f ->
      impliesIntro "b" \b ->
      forward $
      impliesElim
        (forAllElim (assumption f) bot)
        (forward $ assumption b)


proofSpec :: TestTree
proofSpec =
  testGroup "Proofs"
  [ symSpec
  , transSpec
  , exFalsoSpec 
  , orElimIdemSpec
  , existsIntroSpec
  , andCommSpec
  , forallElimSpec
  -- More tests here!
  ]

kernelSpec :: TestTree
kernelSpec =
  testGroup "Kernel"
  [ typecheckSpec
  , proofSpec
  ]
