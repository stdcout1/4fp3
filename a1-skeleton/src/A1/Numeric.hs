-- | Numeric differentiation.
module A1.Numeric
  ( delta
  , ntimes
  , taylor
  ) where

import A1.Polynomial

import Numeric.Natural

-- we just implemenet the fomula given in the document 
-- | The finite difference operator @Δₕ(f)@.
delta :: Rational -> (Rational -> Rational) -> (Rational -> Rational)
delta h f x = (f (x + h) - f x ) / h   

-- | @taylor h n f a@ computes the @n@th degree Taylor expansion of @f@ at @a@ with step size @h@.
taylor :: Rational -> Natural -> (Rational -> Rational) -> Rational -> Poly
-- the "0th term is f(a)"
taylor _ 0 f a = Const (f a)
-- we make a design choice: f(a) + ... 
taylor h n f a = taylor h (n - 1) f a --recusivly do the left side of the plus 
                `Add` -- then add it to the nth term  
                    (
                        ( -- this is the constant and the x^n
                            (X `Pow` n) 
                            `Mul`
                            Const (1.0 / fromIntegral (factorial n))
                        ) 
                        `Mul`
                        Const (ntimes n (delta h) f a)
                        -- we componse (delta h) n times. 
                        -- consider the type 
                        -- delta h : (Rational -> Rational) -> (Rational -> Rational)
                        --              a                   ->      a 
                        -- so n times "preserves this type" and returns the exact signiture just applied n times 
                        -- then we apply f and a, as normal
                    ) 
                     

--------------------------------------------------------------------------------
-- Helper functions: you can use these in your solution.

-- | The @n@-fold iteration of a function @f@.
--
-- >>> ntimes 3 (+2) 1
-- 7
ntimes :: Natural -> (a -> a) -> (a -> a)
ntimes 0 f = id
ntimes n f = f . ntimes (n - 1) f

-- | The @n@th factorial.
--
-- >>> factorial 4
-- 24
factorial :: Natural -> Natural
factorial 0 = 1
factorial n = n * factorial (n - 1)
