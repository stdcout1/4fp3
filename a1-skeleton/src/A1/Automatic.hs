-- | Differentiation using dual numbers.
module A1.Automatic
  ( Dual(..)
  , ratDual
  , addDual
  , mulDual
  , powDual
  , evalDual
  ) where

import A1.Polynomial
import Numeric.Natural

data Dual = Dual Rational Rational
  deriving (Show, Eq)

-- for this file, we basically just follow the rules given in the document... 
-- nothing to really explain

-- | Lift a rational to a dual number.
ratDual :: Rational -> Dual
ratDual a = Dual a 0

-- | Add two dual numbers.
addDual :: Dual -> Dual -> Dual
addDual (Dual a a') (Dual b b') = Dual (a + b) (a' + b') 

-- | Multiply two dual numbers.
mulDual :: Dual -> Dual -> Dual
mulDual (Dual a a') (Dual b b') = Dual (a * b) (a * b' + a' * b) 

-- | Take the @n@th power of a dual number.
powDual :: Dual -> Natural -> Dual
powDual _ 0 = Dual 1 0
powDual (Dual a a') n = Dual (a ^ n) (toRational n * a^(n-1) * a')

-- | Evaluate a polynomial at a dual number.
evalDual :: Poly -> Dual -> Dual
evalDual X x = x
evalDual (Const a) x = ratDual a
evalDual (Add p1 p2) x = evalDual p1 x `addDual` evalDual p2 x
evalDual (Mul p1 p2) x = evalDual p1 x `mulDual` evalDual p2 x
evalDual (Pow p n) x = powDual (evalDual p x) n
