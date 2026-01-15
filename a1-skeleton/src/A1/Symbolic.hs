-- | Symbolic differentiation.
module A1.Symbolic
  ( sdiff
  )
  where

import A1.Polynomial

-- | Differentiate a polynomial with respect to a variable.
--
-- >>> sdiff (Add (Pow (Add X (Const (2 / 3))) 2) (Mul (Const (- 2)) X))
-- Add (Mul (Mul (Const (2 % 1)) (Pow (Add X (Const (2 % 3))) 1)) (Add (Const (1 % 1)) (Const (0 % 1)))) (Add (Mul (Const (0 % 1)) X) (Mul (Const ((-2) % 1)) (Const (1 % 1))))
sdiff :: Poly -> Poly
sdiff X = Const 1
sdiff (Const _) = Const 0 
sdiff (Add p1 p2) = sdiff p1 `Add` sdiff p2
-- product rule f'g + fg'
sdiff (Mul p1 p2) = (sdiff p1 `Mul` p2) `Add` (p1 `Mul` sdiff p2)
-- power rule 
sdiff (Pow p1 n) = Const (toRational n) `Mul` (p1 `Pow` (n-1)) `Mul` sdiff p1
