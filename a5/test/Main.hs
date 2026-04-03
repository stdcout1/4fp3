-- |
module Main (main) where

import Test.Tasty

import Spec.Compile
import Spec.Eval.Pure
import Spec.Eval.Partial
import Spec.Poly.Sparse

main :: IO ()
main = defaultMain $
  testGroup "A5"
  [ evalPureSpec
  , compileSpec
  , evalPartialSpec
  , sparsePolySpec
  ]
