module A1.B3
(
    hyperDualf',
    hyperDualf''
) where

import A1.Polynomial


-- x = a0 + a1ε1 + a2ε2 + a21ε1ε2 
data HyperDual = HyperDual Rational Rational Rational Rational

--as duals encode first order derivatives, HyperDuals encode the second & first 
--but first some basics, kim & park's paper have these  

--real part of a hyperdual. used for control folow?
fromHyperDual :: HyperDual -> Rational
fromHyperDual (HyperDual a0 _ _ _ ) = a0

--extraction of coeffecient of ε1 ε2, and ε12
extractNilpotent1 :: HyperDual -> Rational
extractNilpotent1 (HyperDual _ a1 _ _ ) = a1
extractNilpotent2 :: HyperDual -> Rational
extractNilpotent2 (HyperDual _ _ a2 _ ) = a2
extractNilpotent12 :: HyperDual -> Rational
extractNilpotent12 (HyperDual _ _ _ a12 ) = a12

-- add two hyperduals 
addHyperDual :: HyperDual -> HyperDual -> HyperDual
addHyperDual (HyperDual a0 a1 a2 a12) (HyperDual b0 b1 b2 b12) = 
    HyperDual (a0 + b0) (a1 + b1) (a2 + b2) (a12 + b12)

-- multuply hyperduals 
mulHyperDual :: HyperDual -> HyperDual -> HyperDual
mulHyperDual (HyperDual a0 a1 a2 a12) (HyperDual b0 b1 b2 b12) = 
    HyperDual 
        (a0*b0)
        (a0*b1 + a1*b0)
        (a0*b2 + a2*b0)
        (a0*b12 + a1 * b2 + a2 * b1 + a12 * b0)

-- inverse if a_0 \neq 0 


-- functions must be smooth... how to specifcy in haskell? c^2(R)
-- well we know polynomials are smooth..
-- this is f(x + d) where d is the hyper-dual perturbation in the paper. we choose h1 & h2 = 0 for simplicity
secondOrderLift :: Rational -> Poly -> HyperDual
secondOrderLift x0 X = HyperDual x0 1 1 0 
secondOrderLift _ (Const n) = HyperDual n 0 0 0
secondOrderLift x0 (Add p1 p2) = addHyperDual (secondOrderLift x0 p1)  (secondOrderLift x0 p2)
secondOrderLift x0 (Mul p1 p2) = mulHyperDual (secondOrderLift x0 p1)  (secondOrderLift x0 p2)
secondOrderLift x0 (Pow p1 2) = secondOrderLift x0 (Mul p1 p1) 
secondOrderLift x0 (Pow p1 n) = secondOrderLift x0 (Pow p1 (n-1)) `mulHyperDual` secondOrderLift x0 p1 

hyperDualf' :: Rational -> Poly -> Rational
hyperDualf' x f = extractNilpotent1 $ secondOrderLift x f


hyperDualf'' :: Rational -> Poly -> Rational
hyperDualf'' x f = extractNilpotent12 $ secondOrderLift x f



