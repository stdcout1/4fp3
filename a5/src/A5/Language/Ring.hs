{- HLINT ignore "Use -" -}
module A5.Language.Ring
  ( Ring(..)
  , Monus(..)
  , One(..)
  , defaultPow
  ) where

import Prelude hiding ((+), (-), negate, (*), (^))
import Prelude qualified as P
import GHC.Natural
import Data.Semigroup

class Monoid m => Monus m where
  monus :: m -> m -> m

class Monoid m => One m where
  one :: m

  fromNatural :: Natural -> m
  fromNatural n = stimes n one

class Ring a where
  {-# MINIMAL int, (+), (*), ((-) | negate) #-}
  int :: Int -> a
  (+) :: a -> a -> a
  (*) :: a -> a -> a

  (-) :: a -> a -> a
  x - y = x + negate y

  negate :: a -> a
  negate x = int 0 - x

  (^) :: a -> Word -> a
  (^) = defaultPow

infixl 6 +, -
infixl 7 *
infixl 8 ^

defaultPow :: (Ring a) => a -> Word -> a
defaultPow _ 0 = int 1
defaultPow x 1 = x
defaultPow x n =
    let (d, r) = n `divMod` 2
        xd = defaultPow x d
    in if r == 0 then
      xd * xd
    else
      x * xd * xd

instance Ring Int where
  int n = n
  (+) = (P.+)
  (*) = (P.*)
  (-) = (P.-)
  negate = P.negate
  (^) = (P.^)

instance Ring Bool where
  int = odd
  (+) = (/=)
  (*) = (&&)
  (-) = (/=)
  negate = id
  _ ^ 0 = True
  b ^ _ = b

instance (Integral a, Ord a) => Monus (Sum a) where
  x `monus` y
    | x <= y = 0
    | otherwise = Sum (getSum x P.- getSum y)

instance (Integral a) => One (Sum a) where
  one = 1
  fromNatural = fromIntegral

instance Monus Any where
  x `monus` y = Any (getAny x > getAny y)

instance One Any where
  one = Any True
  fromNatural n = Any (n > 0)
