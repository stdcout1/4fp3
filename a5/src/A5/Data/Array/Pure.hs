{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Pure dynamic arrays.
--
-- You do not need to modify this code, but you will need to call it!
module A5.Data.Array.Pure
  ( Array
  , empty
  , singleton
  , push
  , fromList
  , replicate
  , toList
  -- * Queries
  , modifyF
  , modifyBackF
  , modify
  , modifyBack
  , get
  , getBack
  , set
  , setBack
  ) where

import Prelude hiding (replicate)

import Control.Applicative hiding (empty)

import Data.Functor
import Data.Functor.Identity
import Data.Monoid

import Data.List qualified as List

data Tree a
  = Node (Tree a) (Tree a)
  | Leaf ~a
  deriving (Show, Eq, Ord, Functor)

data Digits a
  = One (Tree a) (Digits a)
  | Zero (Digits a)
  | Nil
  deriving (Show, Eq, Ord, Functor)

data Array a = Array Int (Digits a)
  deriving (Eq, Ord)

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node l r) = foldr f (foldr f b r) l

instance Foldable Digits where
  foldr _ b Nil = b
  foldr f b (Zero ds) = foldr f b ds
  foldr f b (One tree ds) = foldr f (foldr f b ds) tree

instance Foldable Array where
  foldr f b (Array _ xs) = foldr f b xs

  null (Array size _) = size == 0
  length (Array size _) = size

toList :: forall a. Array a -> [a]
toList = foldr (:) []

instance (Show a) => Show (Array a) where
  showsPrec prec arr =
    showParen (prec > appPrec) $ showString "fromList " . showList (toList arr)
    where
      appPrec = 10

--------------------------------------------------------------------------------
-- Creation

empty :: forall a. Array a
empty = Array 0 Nil

singleton :: forall a. a -> Array a
singleton x = Array 1 (One (Leaf x) Nil)

-- | Add a new element to the start of an array.
push :: forall a. a -> Array a -> Array a
push a (Array size ds) = Array (size + 1) (loop (Leaf a) ds)
  where
    loop :: Tree a -> Digits a -> Digits a
    loop xs Nil = One xs Nil
    loop xs (Zero ds) = One xs ds
    loop xs (One ys ds) = Zero (loop (Node xs ys) ds)
{-# INLINE push #-}

fromList :: forall a. [a] -> Array a
fromList = foldr push empty
{-# INLINE fromList #-}

replicate :: forall a. Int -> a -> Array a
replicate n a = fromList (List.replicate n a)
{-# INLINE replicate #-}

--------------------------------------------------------------------------------
-- Queries

modifyF :: forall f a. (Applicative f) => Int -> (a -> f a) -> Array a -> f (Array a)
modifyF i f (Array size ds) = Array size <$> alterDigits i 1 ds
  where
    alterDigits :: Int -> Int -> Digits a -> f (Digits a)
    alterDigits _i _size Nil = pure Nil
    alterDigits i !size (Zero ds) = Zero <$> alterDigits i (2 * size) ds
    alterDigits i size (One tree ds)
      | i < size =  alterTree i size tree <&> (`One` ds)
      | otherwise = (tree `One`) <$> alterDigits (i - size) (2 * size) ds


    alterTree :: Int -> Int -> Tree a -> f (Tree a)
    alterTree i _size (Leaf a)
      | i == 0 = Leaf <$> f a
      | otherwise = pure (Leaf a)
    alterTree i size (Node l r) =
      let childSize = size `div` 2
      in if i < childSize then
        alterTree i childSize l <&> (`Node` r)
      else
        (l `Node`) <$> alterTree (i - childSize) childSize r
{-# INLINE modifyF #-}

-- | Modify an array, starting from the back.
modifyBackF :: forall f a. (Applicative f) => Int -> (a -> f a) -> Array a -> f (Array a)
modifyBackF i f arr@(Array size _) = modifyF (size - i - 1) f arr
{-# INLINE modifyBackF #-}

modify :: forall a. Int -> (a -> a) -> Array a -> Array a
modify i f = runIdentity . modifyF i (Identity . f)
{-# INLINE modify #-}

modifyBack :: forall a. Int -> (a -> a) -> Array a -> Array a
modifyBack i f = runIdentity . modifyBackF i (Identity . f)

get :: forall a. Int -> Array a -> Maybe a
get i arr = getFirst $ getConst $ modifyF i (Const . First . Just) arr
{-# INLINE get #-}

getBack :: forall a. Int -> Array a -> Maybe a
getBack i arr = getFirst $ getConst $ modifyBackF i (Const . First . Just) arr
{-# INLINE getBack #-}

set :: forall a. Int -> a -> Array a -> Maybe (Array a)
set i a = modifyF i (\_ -> Just a)
{-# INLINE set #-}

setBack :: forall a. Int -> a -> Array a -> Maybe (Array a)
setBack i a = modifyBackF i (\_ -> Just a)
{-# INLINE setBack #-}
