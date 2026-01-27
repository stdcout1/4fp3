module A1.B1 
(
    standardize
) where 

import A1.Polynomial

distrib :: Poly -> Poly -> Poly
distrib (Add a b) t = Add (distrib a t) (distrib b t)      -- (a+b)*t = a*t + b*t
distrib t (Add a b) = Add (distrib t a) (distrib t b)      -- t*(a+b) = t*a + t*b
distrib a b         = Mul a b                               -- base: can't distribute further

standardize :: Poly -> Poly 
standardize X = X 
standardize (Const n) = Const n
standardize (Add l r) = Add l r
standardize (Mul l r) = distrib l r
standardize (Pow t 2) = standardize ( t `Mul` t)
standardize (Pow t n) = standardize ( Pow t (n-1) `Mul` t)
