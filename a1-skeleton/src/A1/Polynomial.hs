-- | Univariate polynomials.
module A1.Polynomial
  ( Poly(..)
  , eval
    -- * Convenience functions
  , pretty
  ) where

import Data.Ratio
import Numeric.Natural

-- | Univariate polynomials.
data Poly
  = X
  -- ^ A variable @x@.
  | Const Rational
  -- ^ A constant polynomial @n@.
  | Add Poly Poly
  -- ^ Addition of polynomials @p1 + p2@.
  | Mul Poly Poly
  -- ^ Multiplication of polynomials @p1 * p2@.
  | Pow Poly Natural
  -- ^ Powers @p^n@.
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate a polynomial @p@ at a point @x@.
--
-- >>> eval (Add (Pow (Add X (Const (2 / 3))) 2) (Mul (Const (- 2)) X)) 2
-- 28 % 9
eval :: Poly -> Rational -> Rational
eval X x = x
eval (Const n) _ = n
eval (Add p1 p2) x = eval p1 x + eval p2 x
eval (Mul p1 p2) x = eval p1 x * eval p2 x
eval (Pow p1 n) x = eval p1 x ^ n  

--------------------------------------------------------------------------------
-- Convenience functions
-- You do not need to edit or use these functions: they are
-- just here for your convienence!

-- | Pretty-print a polynomial.
--
-- >>> pretty (Add (Pow (Add X (Const (2 / 3))) 2) (Mul (Const (- 2)) X))
-- "(x + 2/3)^2 + -2*x"
pretty :: Poly -> String
pretty p = loop 0 p ""
  where
    addPrec = 0
    divPrec = 1
    mulPrec = 2
    expPrec = 3

    loop :: Int -> Poly -> ShowS
    loop env X = showString "x"
    loop env (Const a)
      | denominator a == 1 = shows (numerator a)
      | otherwise = showParen (env > divPrec) (shows (numerator a) . showString "/" . shows (denominator a))
    loop env (Add p1 p2) = showParen (env > addPrec) (loop 0 p1 . showString " + " . loop 0 p2)
    loop env (Mul p1 p2) = showParen (env > mulPrec) (loop mulPrec p1 . showString "*" . loop mulPrec p2)
    loop env (Pow p n) = showParen (env > expPrec) (loop expPrec p . showString "^" . shows n)
