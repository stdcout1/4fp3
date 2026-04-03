{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Avoid lambda" -}
-- | A simple imperative language.
module A5.Language
  ( Lang(..)
  -- * Arithmetic
  , Arith(..)
  , pow
  -- * Booleans
  , Logic(..)
  ) where

import Prelude hiding (not, or, and)

import A5.Language.Ring (Ring)
import A5.Language.Ring qualified as R

-- | A simple programming language.
class Lang val cmp | cmp -> val, val -> cmp where
  -- | Integer literals.
  int :: Int -> val

  -- | Addition.
  add :: val -> val -> cmp

  -- | Subtraction.
  sub :: val -> val -> cmp

  -- | Multiplication.
  mul :: val -> val -> cmp

  -- | Boolean literals.
  bool :: Bool -> val

  -- | Negation of a boolean.
  not :: val -> cmp

  -- | Conjunction of booleans.
  and :: val -> val -> cmp

  -- | Disjunction of booleans.
  or :: val -> val -> cmp

  -- | Exclusive or booleans.
  xor :: val -> val -> cmp

  -- | Less-than or equal on integers.
  leq :: val -> val -> cmp

  -- | Less-than on integers.
  lt :: val -> val -> cmp

  -- | Create a new immutable array.
  array :: [val] -> val

  -- | Index into an immutable array.
  --
  -- @xs[i]@.
  getArray :: val -> val -> cmp

  -- | Set a value in an immutable array, returning
  -- the new array.
  --
  -- @xs[i] := v@
  setArray :: val -> val -> val -> cmp

  -- | Get the length of an immutable array.
  lenArray :: val -> cmp

  -- | Sequencing.
  let_ :: cmp -> (val -> cmp) -> cmp

  -- | A computation that returns a pure value.
  ret :: val -> cmp

  -- | A for loop counting up, along with an accumulator that gets threaded
  -- through for every iteration.
  --
  -- The lower bound of the loop is inclusive, and the upper bound of the loop is exclusive.
  --
  -- As an example, the program
  --
  -- @
  -- forDown 0 4 (array [0,0,0,0]) \i acc -> setArray acc i i
  -- @
  --
  -- Returns the array @[0, 1, 2, 3]@
  forUp :: (Lang val cmp) => val -> val -> val -> (val -> val -> cmp) -> cmp

  -- | A for loop counting down, along with an accumulator that gets threaded
  -- through for every iteration. As an example, the program
  --
  -- Both the lower and upper bounds are inclusive.
  --
  -- @
  -- forDown 3 0 (array [0,0,0,0]) \i acc -> setArray acc i i
  -- @
  --
  -- Returns the array @[0, 1, 2, 3]@
  forDown :: (Lang val cmp) => val -> val -> val -> (val -> val -> cmp) -> cmp

  -- | Conditionals
  if_ :: val -> cmp -> cmp -> cmp

  -- | Exceptions.
  crash :: String -> cmp

--------------------------------------------------------------------------------
-- Arithmetic

-- | Newtype for integer computations.
newtype Arith cmp = Arith { runArith :: cmp }

instance (Lang val cmp) => Ring (Arith cmp) where
  int n = Arith (ret (int n))
  cx + cy = Arith $
    let_ (runArith cx) \x ->
    let_ (runArith cy) \y ->
    x `add` y
  cx - cy = Arith $
    let_ (runArith cx) \x ->
    let_ (runArith cy) \y ->
    x `sub` y
  cx * cy = Arith $
    let_ (runArith cx) \x ->
    let_ (runArith cy) \y ->
    x `mul` y

  cx ^ n = Arith $
    let_ (runArith cx) \x ->
    pow x n

-- | Powers.
pow :: (Lang val cmp) => val -> Word -> cmp
pow x 0 = ret (int 1)
pow x 1 = ret x
pow x 2 = mul x x
pow x n =
  let (d, r) = n `divMod` 2
  in if r == 0 then
    let_ (pow x d) \xd -> mul xd xd
  else
    let_ (pow x d) \xd ->
    let_ (mul xd xd) \xdSquare ->
    mul x xdSquare

--------------------------------------------------------------------------------
-- Booleans

-- | Newtype for boolean computations.
newtype Logic cmp = Logic { runLogic :: cmp }

instance (Lang val cmp) => Ring (Logic cmp) where
  int n = Logic (ret (bool (n > 0)))
  cx + cy = Logic $
    let_ (runLogic cx) \x ->
    let_ (runLogic cy) \y ->
    x `xor` y
  cx * cy = Logic $
    let_ (runLogic cx) \x ->
    let_ (runLogic cy) \y ->
    x `and` y

  negate cx = cx
