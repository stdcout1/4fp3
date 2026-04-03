{-# LANGUAGE NoImplicitPrelude #-}
{- HLINT ignore "Use const" -}
{- HLINT ignore "Avoid lambda" -}

-- | Notation helpers for @A5.Language@.
module A5.Language.Notation
  ( (>>=), (>>)
  -- * Integers
  , fromInteger
  , negate
  , (+), (-), (*)
  -- * Booleans
  , true, false
  , (&&), (||)
  , (<=), (>=), (<), (>)
  , ifThenElse
  , P.String
  , fromString
  ) where

import Prelude (Integer)
import Prelude qualified as P

import A5.Language

fromString :: P.String -> P.String
fromString = P.id

(>>=) :: (Lang val cmp) => cmp -> (val -> cmp) -> cmp
(>>=) = let_
{-# INLINE (>>=) #-}

(>>) :: (Lang val cmp) => cmp -> cmp -> cmp
c1 >> c2 = c1 >>= \_ -> c2
{-# INLINE (>>) #-}

--------------------------------------------------------------------------------
-- Integers

fromInteger :: (Lang val cmp) => Integer -> val
fromInteger n = int (P.fromInteger n)
{-# INLINE fromInteger #-}

(+) :: (Lang val cmp) => val -> val -> cmp
(+) = add
{-# INLINE (+) #-}

(-) :: (Lang val cmp) => val -> val -> cmp
x - y = sub x y
{-# INLINE (-) #-}

negate :: (Lang val cmp) => val -> cmp
negate = sub (int 0)
{-# INLINE negate #-}

(*) :: (Lang val cmp) => val -> val -> cmp
(*) = mul
{-# INLINE (*) #-}

--------------------------------------------------------------------------------
-- Booleans

true :: (Lang val cmp) => val
true = bool P.True
{-# INLINE true #-}

false :: (Lang val cmp) => val
false = bool P.False
{-# INLINE false #-}

(&&) :: (Lang val cmp) => val -> val -> cmp
(&&) = and
{-# INLINE (&&) #-}

infixr 3 &&

(||) :: (Lang val cmp) => val -> val -> cmp
(||) = or
{-# INLINE (||) #-}

infixr 2 ||

(<=) :: (Lang val cmp) => val -> val -> cmp
(<=) = leq
{-# INLINE (<=) #-}

(>=) :: (Lang val cmp) => val -> val -> cmp
x >= y = y <= x
{-# INLINE (>=) #-}

(<) :: (Lang val cmp) => val -> val -> cmp
x < y = lt x y
{-# INLINE (<) #-}

(>) :: (Lang val cmp) => val -> val -> cmp
x > y = y < x
{-# INLINE (>) #-}

infix 4 <=, >=, <, >

ifThenElse :: (Lang val cmp) => val -> cmp -> cmp -> cmp
ifThenElse = if_
{-# INLINE ifThenElse #-}
