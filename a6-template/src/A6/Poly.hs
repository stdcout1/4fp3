module A6.Poly
  ( Poly (..),
    poly,
  )
where

-- | Polynomial represented as a list of coefficients: $[x^0, x^1, x^2 ...]$
newtype Poly = Poly {unPoly :: [Rational]}
  deriving (Eq, Show)

poly :: [Rational] -> Poly
poly = Poly . normalize


-- | Strip trailing zero coefficients
normalize :: [Rational] -> [Rational]
normalize = reverse . dropWhile (== 0) . reverse

-- | Add terms of two polynomials.
addTerms :: (Num a) => [a] -> [a] -> [a]
addTerms [] ys = ys
addTerms xs [] = xs
addTerms (x : xs) (y : ys) = (x + y) : addTerms xs ys

instance Num Poly where
  negate = Poly . map negate . unPoly

  Poly xs + Poly ys = poly $ addTerms xs ys

  Poly [] * _ = Poly []
  Poly xs * Poly ys = poly $ foldr1 addTerms terms
    where
      terms = zipWith (\i x -> replicate i 0 ++ map (* x) ys) [0 ..] xs

  fromInteger n = Poly [fromInteger n] 
  abs = undefined
  signum = undefined
