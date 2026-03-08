module PriorityQueue
  ( Heap,
    empty,
    isEmpty,
    singleton,
    isSingleton,
    insert,
    merge,
    findMin,
    deleteMin,
    popMin,
    fromList,
    toPrioList,
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.List (sort)
import Data.Maybe (fromMaybe)

-- | The Heap data structure.
--
-- The 'Int' field tracks the weight (total number of nodes) of the subtree.
--
-- Invariant: For any @Node w _ l r@, @weight l >= weight r@, ensuring the right
-- spine length is bounded by \(O(\log{}(N))\).
--
-- Follows Okasaki's Weight-Biased Leftist Heap (Ch. 3 of "Purely Functional
-- Data Structures").
data Heap a
  = Empty
  | Node !Int !a !(Heap a) !(Heap a)
  deriving (Show, Eq)


-- | The empty heap. \(O(1)\).
empty :: Heap a
empty = Empty

-- | Check if the heap is empty. \(O(1)\).
isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | Create a singleton heap. \(O(1)\).
singleton :: a -> Heap a
singleton x = Node 1 x Empty Empty

-- | Check if the heap is a singleton, returning the element if it is. \(O(1)\).
isSingleton :: Heap a -> Maybe a
isSingleton (Node _ i Empty Empty) = Just i
isSingleton _ = Nothing

-- | Get the weight (or, node count) of a heap. \(O(1)\).
weight :: Heap a -> Int
weight Empty = 0
weight (Node w _ _ _) = w

-- | Smart constructor for 'Node's that enforces the leftist property by weight.
-- \(O(1)\).
-- leftist property: the rank (legnth of the rightmost path to empty) of a left child 
-- is atleast as large (greater than or eq) as the rank of the right sibling. 
buildNode :: a -> Heap a -> Heap a -> Heap a
buildNode i left right =
--  basically check if left is atleast as large as right. 
--  if it is we add a node then set w to be sum of children + 1 
--  if not, we add a node but make sure to swap them. 
    if lw >= rw then 
        Node (rw + lw + 1) i left right 
    else 
        Node (rw + lw + 1) i right left 
    where 
        lw = weight left 
        rw = weight right

-- | Merge two heaps, retaining duplicates. \(O(\log{}(N))\).
-- Recursively merges along the right spines of both heaps.
--
-- >>> let h1 = fromList [3, 5]
-- >>> let h2 = fromList [2, 1]
-- >>> let h3 = merge h1 h2
-- >>> h3
-- >>> findMin h3
-- Node 4 1 (Node 2 3 (Node 1 5 Empty Empty) Empty) (Node 1 2 Empty Empty)
-- Just 1
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge h1@(Node _ x l1 r1) h2@(Node _ y l2 r2) = 
    if x <= y then
        -- x <= y so we make x the root of y. 
        -- do this by: 
        --      make a node x. 
        --      one branch should be l1. 
        --      other branch is merging the r1, h2 as the value of h2 may be smaller than r1
        buildNode x l1 (merge r1 h2)
    else 
        -- opposite of above
        buildNode y l2 (merge h1 r2)

-- | Insert a single element into a heap. \(O(\log{}(N))\).
insert :: (Ord a) => a -> Heap a -> Heap a
insert i old = 
    -- add a empty node and merge it into the rest 
    merge new old 
    where new = Node 1 i Empty Empty

-- | Peek at the minimum element. \(O(1)\).
findMin :: Heap a -> Maybe a
findMin Empty = Nothing
findMin (Node _ i _ _) = 
    -- min the just the root 
    Just i

-- | Remove the minimum element from a heap. \(O(\log{}(N))\).
deleteMin :: (Ord a) => Heap a -> Maybe (Heap a)
deleteMin Empty = Nothing
-- remove root and merge children 
deleteMin (Node _ _ l r) = Just $ merge l r 


-- | Remove and return the minimum element. \(O(\log{}(N))\).
popMin :: (Ord a) => Heap a -> Maybe (a, Heap a)
popMin h = do 
    m <- findMin h
    n <- deleteMin h 
    Just (m, n)
-- deleteMin and return min. if any return nothing propagate it up using monads

-- | Build a heap from a list of elements. \(O(N\log{}(N))\).
fromList :: (Ord a) => [a] -> Heap a
fromList list =
    let 
        builder (x : xs) h = insert x (builder xs h)
        builder [] _ = Empty
    in 
        builder (sort list) Empty
-- sort the list in increasing order (n log n) and insert it in that order (n inserts each being log n) 
-- so total is O(n log n)

-- | Dump a heap to a sorted list (ascending order). \(O(N\log{}(N))\).
toPrioList :: (Ord a) => Heap a -> [a]
toPrioList h =
    let 
        builder :: (Ord a) => Heap a -> Maybe [a]
        builder h1 = do 
            -- this only fails on first pass but i dont like handling it
            (m, n) <- popMin h1
            case builder n of 
                Just next -> Just (m : next) 
                Nothing -> Just [m]
    in fromMaybe [] (builder h) 
    -- n insertions which require a popMin of log n time -> n log n
