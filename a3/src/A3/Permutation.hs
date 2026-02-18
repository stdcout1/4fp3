{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Permutations.
--
-- This module is indended to be imported qualified as
-- > import A3.Syntax.Permutation (Permutation)
-- > import A3.Syntax.Permutation qualified as P
module A3.Permutation
  ( Permutation
  , permute
  , support
  , swap
  -- $predicates
  , isIdentity
  -- $cycles
  , splitCycle
  , deleteCycle
  , deleteCycles
  , toCycles
  , fromCycle
  , fromCycles
  -- $unsafeFunctions
  , unsafeFromCycle
  , unsafeFromCycles
  , debugShow
  ) where


import Data.List (foldl')
import Data.Containers.ListUtils qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | A finitely supported permutation.
newtype Permutation a
  = Permutation { getPermutation :: Map a a }
  -- ^ We represent a permutation as a @p :: Map a a@ with the following
  -- invariants:
  --
  -- 1. @Map.member y p@ if and only if there is some @x@ such that @Map.lookup x p == Just y@
  -- 2. @Map.lookup x p /= x@.
  deriving (Eq)

instance (Show a, Ord a) => Show (Permutation a) where
  show p = "fromCycles " <> show (toCycles p)

-- | Apply a permutation to an element.
permute :: (Ord a) => a -> Permutation a -> a
permute x (Permutation p) = Map.findWithDefault x x p

-- * The support of a permutation, EG: the elements where @p x /= x@.
support :: Permutation a -> Set a
support (Permutation p) = Map.keysSet p

-- | The permutation that swaps a pair of elements.
swap :: (Ord a) => a -> a -> Permutation a
swap x y = Permutation (Map.fromList [(x, y), (y, x)])

instance (Ord a) => Semigroup (Permutation a) where
  -- the error/bug is here, we accidently swapped p1 and p2. 
  p1 <> p2 = Permutation $
    Map.fromList [ (i, permute (permute i p1) p2) | i <- Set.toList (support p1 `Set.union` support p2) ]
  -- Permutation p1 <> Permutation p2 = Permutation $ p1 <> p2

instance (Ord a) => Monoid (Permutation a) where
  mempty = Permutation Map.empty

-- $predicates
-- * Predicates

-- | Is this permutation the identity permutation.
isIdentity :: Permutation a -> Bool
isIdentity (Permutation p) =
  -- This works because of invariant (3), which ensures
  -- that the identity permutation has a unique representation.
  Map.null p


-- $cycles
-- * Cycles

-- | Split a permutation into a cycle containing some @x@ and
-- the rest of the permutation.
splitCycle :: forall a. (Ord a) => a -> Permutation a -> ([a], Permutation a)
splitCycle start (Permutation p) =
  case Map.alterF deleteOrInsert start p of
    (Just px, p') ->
      let ~(cyc, p'') = followCycle px p'
      in (start:cyc, Permutation p'')
    (Nothing, _) -> ([], Permutation p)
  where
    deleteOrInsert :: Maybe a -> (Maybe a, Maybe a)
    deleteOrInsert (Just x) = (Just x, Nothing)
    deleteOrInsert Nothing = (Nothing, Nothing)

    followCycle :: a -> Map a a -> ([a], Map a a)
    followCycle x p =
      case Map.alterF deleteOrInsert x p of
        (Just px, p') | px == start -> ([x], Map.delete x p)
                      | otherwise ->
                   -- Lazy match so that we can consume the cycle lazily.
                   let ~(cyc, p'') = followCycle px p' in (x:cyc, p'')
        (Nothing, _) ->
          -- This violates invariant (1)
          error "splitCycle: invariant violated."

-- | Remove the cycle starting with @x@ from a permutation.
deleteCycle :: (Ord a) => a -> Permutation a -> Permutation a
deleteCycle x p = snd (splitCycle x p)

-- | Remove all cycles that contain elements in the list @xs@ from a permutation.
deleteCycles :: (Ord a) => [a] -> Permutation a -> Permutation a
deleteCycles xs p = foldl' (flip deleteCycle) p xs

-- | Decompose a permutation into cycles.
toCycles :: forall a. (Ord a) => Permutation a -> [[a]]
toCycles p =
  case peek p of
    Just x ->
      let (cyc, p') = splitCycle x p
      in cyc:toCycles p'
    Nothing -> []
  where
    peek :: Permutation a -> Maybe a
    peek (Permutation p) = fst <$> Map.lookupMin p

fromCycle :: forall a. (Ord a) => [a] -> Permutation a
fromCycle cyc = unsafeFromCycle (List.nubOrd cyc)

fromCycles :: forall a. (Ord a) => [[a]] -> Permutation a
fromCycles = foldMap fromCycle

-- $unsafeFunctions
-- * Unsafe functions
--
-- These functions can violate the internal invariants of @Permutation@,
-- but are typically more efficient.

-- | Create a permutation from a cycle.
--
-- This function does not check that the provided list actually forms a cycle.
unsafeFromCycle :: forall a. (Ord a) => [a] -> Permutation a
unsafeFromCycle [] = mempty
unsafeFromCycle [_x0] = mempty
unsafeFromCycle (x0:x1:xs) = Permutation (loop x0 x1 (Map.singleton x0 x1) xs)
  where
    loop :: a -> a -> Map a a -> [a] -> Map a a
    loop start prev !acc [] = Map.insert prev start acc
    loop start prev acc (x:xs) = loop start x (Map.insert prev x acc) xs

debugShow :: (Show a) => Permutation a -> String
debugShow (Permutation p) = "Permutation {getPermutation = " <> show p <> "}"

-- | Create a permutation from a list of disjoint cycles.
--
-- This function does not check that the provided list actually forms a cycle,
-- nor that the cycles are disjoint.
unsafeFromCycles :: (Ord a) => [[a]] -> Permutation a
unsafeFromCycles cycs = Permutation $ Map.unions $ getPermutation . unsafeFromCycle <$> cycs
