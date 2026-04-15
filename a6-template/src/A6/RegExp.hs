module A6.RegExp
  ( RE (..),
    RESize (..),
    smallest,
    largest,
    genFunc,
    powerSeries,
  )
where

import A6.Poly (Poly (..), poly)
import Numeric.Natural

--------------------------------------------------------------------------------
-- REGULAR EXPRESSIONS
--------------------------------------------------------------------------------

data RE
  = Eps
  | Ch Char
  | RE :+: RE -- or
  | RE :->: RE -- concat
  | Star RE
  deriving (Eq, Show)

-- | The size of the smallest string in the language of a regular expression.
smallest :: RE -> Natural
smallest Eps = 0
smallest (Ch _) = 1
-- a | ab -> smallest is a
smallest (l :+: r) = min (smallest l) (smallest r)
-- a(ab) -> sum of 2
smallest (l :->: r) = smallest l + smallest r 
smallest (Star _) = 0

data RESize = Finite Natural | Infinite
  deriving (Eq, Show)

instance Ord RESize where
  _ <= Infinite = True 
  Infinite <= _ = False 
  Finite n <= Finite m = n <= m 

instance Num RESize where 
    Infinite + _ = Infinite 
    _ + Infinite = Infinite
    Finite n + Finite m = Finite (n + m) 
-- | The size of the largest string in the language of a regular expression, or
-- 'Infinite' if there is no largest string.
largest :: RE -> RESize
largest Eps = Finite 0 
largest (Ch _) = Finite 1 
largest (l :+: r) = max (largest l) (largest r)
largest (l :->: r) = largest l + largest r
largest (Star Eps) = 0
largest (Star _) = Infinite

data RatExpr = RatExpr Poly Poly
  deriving (Eq, Show)

-- i mean these are pretty straight forward looking at :https://math.stackexchange.com/questions/225100/generating-functions-for-context-free-languages
instance Num RatExpr where 
    (RatExpr n1 d1) * (RatExpr n2 d2) = RatExpr (n1 * n2) (d1 * d2) 
    (RatExpr n1 d1) + (RatExpr n2 d2) = RatExpr (n1 * d2 + n2 * d1) (d1 * d2)
    -- a - b = a + negate b cool 
    negate (RatExpr n d) = RatExpr (negate n) d
    fromInteger n = RatExpr (fromInteger n) (Poly [1])

inverse :: RatExpr -> RatExpr
inverse (RatExpr n1 n2) = RatExpr n2 n1

-- | Calculates the ordinary generating function (OGF) for a regular expression,
-- represented as a rational expression (a ratio of two polynomials).
genFunc :: RE -> RatExpr
genFunc Eps = 1 
genFunc (Ch _) = RatExpr (poly [0, 1]) 1 
genFunc (l :+: r) = genFunc l + genFunc r 
genFunc (l :->: r) = genFunc l * genFunc r
genFunc (Star r) = inverse (1 - genFunc r)

x0 :: Poly -> Rational 
x0 (Poly (x : _)) = x
x0 (Poly []) = 0

-- | The first $n$ terms of the power series expansion of the generating
-- function for a given regular expression.
powerSeries :: Int -> RE -> [Rational]
-- polynomial long division. 
-- https://en.wikipedia.org/wiki/Polynomial_long_division#Pseudocode
powerSeries n re = powerSeries' n (genFunc re)  
-- its easier to do this 
powerSeries' :: Int -> RatExpr -> [Rational]
powerSeries' 0 _ = []
powerSeries' n (RatExpr num denmo) = 
    let
        -- basically compute first term the move over and use the remainder
        q1 = x0 num / x0 denmo 
        -- i dont think its safe to cast q1 to int...
        remainder = num - (poly [q1]) * denmo
        move = Poly (drop 1 (unPoly remainder))
    in q1 : (powerSeries' (n - 1) (RatExpr move denmo))

