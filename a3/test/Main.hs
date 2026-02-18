module Main where

import Spec.Permutation
import Spec.Syntax.Core
import Spec.Syntax.Substitution
import Spec.Kernel

import Test.Tasty

main :: IO ()
main = defaultMain $
  testGroup "A3"
  [ permutationSpec
  , coreSyntaxSpec
  , substitutionSpec
  , kernelSpec
  ]
