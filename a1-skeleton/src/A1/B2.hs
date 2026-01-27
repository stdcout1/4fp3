module A1.B2
(
    cstep
) where

import Data.Complex (Complex (..), imagPart)
import A1.Automatic (Dual (Dual))
--------------------------------------------------------------------------------
-- B2 Complex-Step Diffrentiation 
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Complex.html
-- for some reason the assignment specs says to accept x as a complex 
-- and to return the complex while the link provided as source considers 
-- x as a 
cstep :: Double -> (Complex Double -> Complex Double) -> Complex Double -> Complex Double
cstep h f x = let 
    d = imagPart (f (x + (h :+ 0) *(0 :+ 1))) / h 
    in 
    d :+ 0

fromDual :: Double -> Dual -> Complex Double 
fromDual h (Dual a b) = fromRational a :+ fromRational b *h

toDual :: Double -> Complex Double -> Dual 
toDual h (a :+ b) = Dual (toRational a) (toRational (b / h))

sinDual :: Double -> Dual -> Dual 
sinDual h d = toDual h (sin (fromDual h d)) 
