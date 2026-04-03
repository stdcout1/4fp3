{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Spec.Compile
  ( compileSpec
  ) where

import Test.Tasty
import Test.Tasty.HUnit
-- prevent confilic
import Prelude hiding (and, not, or)

import A5.Language
import A5.Compile
import A5.Eval.Pure

-- so the idea here is to 
-- first we need to be able to turn FreshM Prog -> Prog so we can 
-- actualyl compile into Prog which can by run by the pure evaluator
compile :: FreshM Prog -> Prog
compile prog = runFreshM prog 0

-- now we run the program. we have a instance for lang 
-- for some Val and runComputeM prog. so vals are interperters val 
-- and cmps are runComputeM. luckly we have it done for us :D. 
-- so we can just use it. this val is the pure's Val
runP :: FreshM Prog -> Either String Val
runP p =
    -- there is no vars on startup so a look up should always fail
    let look v = error $ "there is no var: " ++ show v
    -- we need to be able to get a computeM Prog. 
    -- this is a cmp on Lang, so we need a way to get cmp on 
    -- arb instances of Lang val cmp. a quick grep into our source shows 
    -- that interpretProg has this signiture
    in runComputeM (interpretProg (compile p) look) []

runM :: ComputeM Val -> Either String Val 
runM m = runComputeM m []

same :: ComputeM Val -> FreshM Prog -> Assertion
same progM progP =
  runP progP @?= runM progM

-- some programs 
progStepLet :: Lang val cmp => cmp
progStepLet =
  let_ (add (int 20) (int 22)) ret

progBoolLet :: Lang val cmp => cmp
progBoolLet =
  let_ (and (bool True) (bool False)) \b ->
    if_ b (ret (int 1)) (ret (int 0))

progNestedLet :: Lang val cmp => cmp
progNestedLet =
  let_ (let_ (add (int 1) (int 2)) ret) \x ->
    mul x (int 10)

progIfLetTrue :: Lang val cmp => cmp
progIfLetTrue =
  let_ (if_ (bool True)
            (add (int 2) (int 3))
            (add (int 100) (int 200))) \x ->
    add x (int 1)

progIfLetFalse :: Lang val cmp => cmp
progIfLetFalse =
  let_ (if_ (bool False)
            (add (int 2) (int 3))
            (add (int 100) (int 200))) \x ->
    sub x (int 50)

progRetLet :: Lang val cmp => cmp
progRetLet =
  let_ (ret (int 9)) \x ->
    mul x (int 7)

progCrashLet :: Lang val cmp => cmp
progCrashLet =
  let_ (crash "boom") \x ->
    add x (int 1)

progArraySetGet :: Lang val cmp => cmp
progArraySetGet =
  let_ (ret (array [int 10, int 20, int 30])) \arr ->
    let_ (setArray arr (int 1) (int 99)) \arr' ->
      getArray arr' (int 1)

progForUpSum :: Lang val cmp => cmp
progForUpSum =
  forUp (int 0) (int 5) (int 0) \i acc ->
    add acc i

progForDownSum :: Lang val cmp => cmp
progForDownSum =
  forDown (int 5) (int 1) (int 0) \i acc ->
    add acc i

progIfThenContinue :: Lang val cmp => cmp
progIfThenContinue =
  let_ (lt (int 3) (int 1)) \b ->
    let_ (if_ b
              (sub (int 20) (int 7))
              (add (int 2) (int 3))) \x ->
      add x (int 8)

compileSpec :: TestTree
compileSpec =
    testGroup "Compile"
    [ testCase "let over Step: add" $
        same
            (progStepLet :: ComputeM Val)
            (progStepLet :: FreshM Prog)

    , testCase "let over Step: boolean op" $
        same
            (progBoolLet :: ComputeM Val)
            (progBoolLet :: FreshM Prog)

    , testCase "let over Let: nested let" $
        same
            (progNestedLet :: ComputeM Val)
            (progNestedLet :: FreshM Prog)

    , testCase "let over If: true branch" $
        same
            (progIfLetTrue :: ComputeM Val)
            (progIfLetTrue :: FreshM Prog)

    , testCase "let over If: false branch" $
        same
            (progIfLetFalse :: ComputeM Val)
            (progIfLetFalse :: FreshM Prog)

    , testCase "let over Ret" $
        same
            (progRetLet :: ComputeM Val)
            (progRetLet :: FreshM Prog)

    , testCase "let over Crash" $
        same
            (progCrashLet :: ComputeM Val)
            (progCrashLet :: FreshM Prog)

    , testCase "array set/get" $
        same
            (progArraySetGet :: ComputeM Val)
            (progArraySetGet :: FreshM Prog)

    , testCase "forUp accumulation" $
        same
            (progForUpSum :: ComputeM Val)
            (progForUpSum :: FreshM Prog)

    , testCase "forDown accumulation" $
        same
            (progForDownSum :: ComputeM Val)
            (progForDownSum :: FreshM Prog)

    , testCase "if after let continuation" $
        same
            (progIfThenContinue :: ComputeM Val)
            (progIfThenContinue :: FreshM Prog)
    ]
