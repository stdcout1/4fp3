{-# OPTIONS_GHC -Wno-name-shadowing #-}
{- HLINT ignore "Use &&" -}

-- | Polynomials.
module A5.Poly.Sparse
  ( Poly
  -- * Smart constructors
  , const
  , monomial
  , id
  -- * Views
  , viewConst
  -- * Arithmetic
  , (+), (-), negate
  , (*), (^)
  -- * Evaluation
  , eval
  -- * Pretty printing
  , prettyHorner
  -- * Debugging
  , valid
  ) where

import Prelude hiding (const, id, (+), (-), (*), negate, (^))

import Data.Semigroup

import A5.Language.Ring

-- | Polynomials, with coefficients in a commutative ring @r@, exponents taken from
-- some monoid @m@ with monus, and variables in @x@
--
-- We represent polynomials using sparse horner normal form.
data Poly r m x
  = Const !r
  -- ^ A constant polynomial.
  | Pow x !m !(Poly r m x) !(Poly r m x)
  -- ^ @x^n * p + q@.
  --
  -- Invariant: @n /= mempty@.
  -- Invariant: @p /= Const 0@
  -- Invariant: @p /= Pow x p' (Const 0)@ for some @p'@.
  -- Invariant: All variables in @p@ must be greater than or equal to @x@.
  -- Invariant: All variables in @q@ must be greater than @x@.
  deriving (Show, Eq, Ord)



-- | Smart constructor for @Pow@.
--
-- Precondition: All variables in @q@ must be greater than @x@.
pow :: (Eq r, Ring r, Monus m, Ord m, One m, Ord x) => x -> m -> Poly r m x -> Poly r m x -> Poly r m x
pow _ n p q | n == mempty = p + q
pow _ _ (Const r) q
  | r == int 0 = q
pow x n (Pow y m px p) q =
  case compare x y of
    LT -> Pow x n (Pow y m px p) q
    EQ | p == int 0 -> Pow x (m <> n) px q
       | otherwise -> Pow x n (Pow x m px p) q
    GT -> Pow y m (pow x n px (Const (int 0))) (pow x n p q)
pow x n p q = Pow x n p q

-- | A constant polynomial.
const :: r -> Poly r m x
const = Const

-- | The polynomial @x^n@.
monomial :: (Ring r, Eq r, Monus m, Ord m, One m, Ord x) => x -> m -> Poly r m x
monomial x n
  | n == mempty = Const (int 0)
  | otherwise = Pow x n (int 1) (int 0)

-- | The identity function.
id :: (Ring r, Eq r, Monus m, One m, Ord m, Ord x) => x -> Poly r m x
id x = monomial x one

--------------------------------------------------------------------------------
-- Views

-- | Check if a polynomial is constant.
viewConst :: Poly r m x -> Maybe r
viewConst (Const r) = Just r
viewConst _ = Nothing

--------------------------------------------------------------------------------
-- Arithmetic

-- | Add a constant.
addScalar :: (Ring r) => r -> Poly r m x -> Poly r m x
addScalar r (Const s) = Const (r + s)
addScalar r (Pow x n p q) = Pow x n p (addScalar r q)

-- | Multiply by a scalar.
mulScalar :: (Eq r, Ring r, Monus m, Ord m, One m, Ord x) => r -> Poly r m x -> Poly r m x
mulScalar r (Const s) = Const (r * s)
mulScalar r (Pow x n px p) = pow x n (mulScalar r px) (mulScalar r p)

instance (Eq r, Ring r, Monus m, One m, Ord m, Ord x) => Ring (Poly r m x) where
  int n = Const (int n)

  Const r + q = addScalar r q
  p + Const r = addScalar r p
  Pow x m px p + Pow y n qy q =
    case compare x y of
      LT -> pow x m px (p + Pow y n qy q)
      GT -> pow y n qy (q + Pow x m px p)
      EQ ->
        case compare m n of
        LT -> pow x m (px + pow x (n `monus` m) qy (int 0)) (p + q)
        GT -> pow x n (pow x (m `monus` n) px (int 0) + qy) (p + q)
        EQ -> pow x m (px + qy) (p + q)

  Const r * q = mulScalar r q
  p * Const s = mulScalar s p


  Pow x m px p * Pow y n qy q =
    case compare x y of
      LT ->
        pow x m (pow y n (px * qy) (int 0)) (pow y n (p * qy) (int 0))
        + pow x m (px * q) (p * q)
      GT ->
        pow y n (pow x m (px * qy) (int 0)) (pow x m (px * q) (int 0))
        + pow y n (p * qy) (p * q)
      EQ ->
        pow x (m <> n) (px * qy) (p * q)
        + pow x m (px * q) (int 0)
        + pow x n (p * qy) (int 0)

  negate (Const r) = Const (negate r)
  negate (Pow x n px p) = Pow x n (negate px) (negate p)

  _ ^ 0 = Const (int 1)
  Const r ^ n = Const (r ^ n)
  Pow x m px (Const r) ^ n | r == int 0 = Pow x (stimes n m) (px ^ n) (Const (int 0))
  p ^ n = defaultPow p n

--------------------------------------------------------------------------------
-- Evaluation

eval :: (Eq r, Ring r, Ring s) => Poly r m x -> (r -> s) -> (s -> m -> s) -> (x -> s) -> s
eval (Const r) fcoeff _fpow _fvar = fcoeff r
eval (Pow x n px p) fcoeff fpow fvar =
  case (viewConst px, viewConst p) of
    (Just spx, Just sp) | spx == int 1 && sp == int 0 -> fpow (fvar x) n
    (Just spx, _)       | spx == int 1 -> fpow (fvar x) n + eval p fcoeff fpow fvar
    (Just _, Just sp)   | sp == int 0 -> fpow (fvar x) n * eval px fcoeff fpow fvar
    (_, _)  -> fpow (fvar x) n * eval px fcoeff fpow fvar + eval p fcoeff fpow fvar

--------------------------------------------------------------------------------
-- Pretty printing

-- | Pretty print the polynomial in Horner normal form.
prettyHorner :: forall r m x. (Show r, Show m, Show x)
  => (Int -> m -> String -> String)
  -> (Int -> x -> String -> String)
  -> Poly r m x
  -> String
prettyHorner pm px p = loop addPrec p ""
  where
    powPrec = 8
    mulPrec = 7
    addPrec = 6

    loop :: Int -> Poly r m x -> String -> String
    loop prec (Const r) = showsPrec prec r
    loop prec (Pow x n p q) =
      showParen (prec > addPrec) $
      px powPrec x . showString "^" . pm powPrec n
      . showString "*" . loop mulPrec p
      . showString " + " . loop (addPrec + 1) q

--------------------------------------------------------------------------------
-- Debugging

-- | Check if all listed invariants hold.
valid :: (Eq r, Ring r, Monoid m, Ord m, Ord x) => Poly r m x -> Bool
valid (Const _) = True
valid (Pow x n px p) = and
  [ n /= mempty
  , validLhs x n px
  , validRhs x p
  ]

-- | Check if all listed invariants of the LHS of a @Pow@.
validLhs :: (Eq r, Ring r, Monoid m, Ord m, Ord x) => x -> m -> Poly r m x -> Bool
validLhs _ _ (Const r) = r /= int 0
validLhs x _n (Pow y m qx q) = and
  [ m /= mempty
  , case compare x y of
      GT -> False
      LT -> True
      EQ -> q /= const (int 0)
  , validLhs y m qx
  , validRhs y q
  ]

-- | Check if all listed invariants of the RHS of a @Pow@.
validRhs :: (Eq r, Ring r, Monoid m, Ord m, Ord x) => x -> Poly r m x -> Bool
validRhs _ (Const _) = True
validRhs x (Pow y m qx q) = and
  [ m /= mempty
  , x < y
  , validLhs y m qx
  , validRhs y q
  ]
