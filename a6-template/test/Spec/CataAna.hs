module Spec.CataAna (cataAnaSpec) where

import A6.CataAna (ExprF (..), freeVars, fromList, toList)
import A6.Fix (Fix (In))
import Data.Set qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

cataAnaSpec :: TestTree
cataAnaSpec =
  testGroup
    "CataAna"
    [ testCase "fromList . toList returns the same list" $
        (toList . fromList $ [1, 2, 3]) @?= [1, 2, 3],
      testCase "Int literal has no free vars" $
        freeVars (In (IntF 42)) @?= S.empty,
      testCase "Single var is free" $
        freeVars (In (VarF "x")) @?= S.singleton "x",
      testCase "Add unions free vars from both sides" $
        freeVars (In (AddF (In (VarF "x")) (In (VarF "y")))) @?= S.fromList ["x", "y"],
      testCase "Mul unions free vars from both sides" $
        freeVars (In (MulF (In (VarF "a")) (In (VarF "b")))) @?= S.fromList ["a", "b"],
      testCase "Let removes binder from body and expression" $
        freeVars (In (LetF "x" (In (IntF 1)) (In (VarF "x")))) @?= S.empty,
      testCase "Let other vars in body remain free" $
        freeVars (In (LetF "x" (In (VarF "y")) (In (VarF "x")))) @?= S.singleton "y",
      testCase "Lambda removes binder from body" $
        freeVars (In (LamF "x" (In (VarF "x")))) @?= S.empty,
      testCase "Lambda other vars in body remain free" $
        freeVars (In (LamF "x" (In (VarF "y")))) @?= S.singleton "y",
      testCase "App unions free vars from function and argument" $
        freeVars (In (AppF (In (VarF "f")) (In (VarF "x")))) @?= S.fromList ["f", "x"],
      testCase "Nested let" $
        freeVars (In (LetF "x" (In (IntF 1)) (In (LetF "z" (In (VarF "y")) (In (AddF (In (VarF "x")) (In (VarF "z")))))))) @?= S.singleton "y",
      testCase "Mix of lambda and app" $
        freeVars (In (AppF (In (LamF "x" (In (AddF (In (VarF "x")) (In (VarF "y")))))) (In (MulF (In (VarF "z")) (In (IntF 2)))))) @?= S.fromList ["y", "z"]
    ]
