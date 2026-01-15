{-# LANGUAGE BlockArguments #-}

-- | See https://hackage-content.haskell.org/package/tasty-hunit-0.10.2/docs/Test-Tasty-HUnit.html
-- for documentation on writing unit tests.
module Main where

import A1.Automatic
import A1.Numeric
import A1.Polynomial
import A1.Symbolic
import Test.Tasty
import Test.Tasty.HUnit

polynomialTests :: TestTree
polynomialTests =
  testGroup
    "Polynomial"
    [ testGroup
        "eval"
        [ testCase "(x + 2/3)^2 + -2*x" do
            eval (Add (Pow (Add X (Const (2 / 3))) 2) (Mul (Const (-2)) X)) 2
              @?= (28 / 9)
        ]
    ]

symbolicTests :: TestTree
symbolicTests =
  testGroup
    "Symbolic"
    [ testGroup
        "sdiff"
        [ testCase "(x + 2/3)^2 + -2*x" do
            sdiff (Add (Pow (Add X (Const (2 / 3))) 2) (Mul (Const (-2)) X))
              @?= Add (Mul (Mul (Const 2) (Pow (Add X (Const (2 / 3))) 1)) (Add (Const 1) (Const 0))) (Add (Mul (Const 0) X) (Mul (Const (-2)) (Const 1)))
        ]
    ]

numericTests :: TestTree
numericTests =
  testGroup
    "Numeric"
    [ testGroup
        "delta"
        [ testCase "Δ_2(f)(3x+2)(3)" do
            delta 2 (eval ((Const 3 `Mul` X) `Add` Const 2)) 3
              @?= 3
        ],
      testGroup
        "taylor"
        [ testCase "2nd-degree Taylor approximation of (3x+2) at 3 (with step size 2)" do
            taylor 2 2 (eval ((Const 3 `Mul` X) `Add` Const 2)) 3
              @?= Add (Add (Const 11) (Mul (Pow X 1) (Mul (Const 1) (Const 3)))) (Mul (Pow X 2) (Mul (Const (1 / 2)) (Const 0)))
        ]
    ]

automaticTests :: TestTree
automaticTests =
  testGroup
    "Automatic"
    [ testGroup
        "addDual"
        [
            testCase "(3, 2) + (2, 3)" do 
                addDual (Dual 3 2) (Dual 2 3) 
                    @?= Dual 5 5
        ],
      -- Your tests here!

      testGroup
        "mulDual"
        [
            testCase "(3, 2) * (2, 3)" do 
                mulDual (Dual 3 2) (Dual 2 3) 
                    @?= Dual (3 * 2) ( 3 * 3 + 2 * 2)
        ],
      -- Your tests here!

      testGroup
        "powDual"
        [
            testCase "(3, 2)^2" do 
                powDual (Dual 3 2) 2 
                    @?= Dual (3^2) (2 * 3^1 * 2)

        ]
    ]

-- Your tests here!

main :: IO ()
main =
  defaultMain $
    testGroup
      "A1"
      [ polynomialTests,
        symbolicTests,
        numericTests,
        automaticTests
      ]
