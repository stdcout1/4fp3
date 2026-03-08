module CharMap
  ( CharMap,
    empty,
    isEmpty,
    singleton,
    isSingleton,
    insertWith,
    insert,
    lookup,
    foldlWithKey',
    fromList,
    toAscList,
  )
where

import Data.Foldable (foldl')
import Prelude hiding (lookup)

-- | Node colour for balancing invariants.
data Colour
  = R
  | B
  deriving (Eq, Show)

-- | Red-Black Tree specialized for 'Char' keys.
--
-- Guarantees \(O(\log{}(A))\) operations (where \(A\) is the alphabet size) by
-- enforcing two structural invariants:
--
-- 1. No red node has a red child.
-- 2. Every path from the root to an empty node contains the same number of
--    black nodes.
--
-- Follows Okasaki's Red-Black Tree (Ch. 3 of "Purely Functional Data
-- Structures").
data CharMap a
  = Empty
  | Node !Colour !(CharMap a) !Char !a !(CharMap a)
  deriving (Eq, Show)

-- | The empty map. \(O(1)\).
empty :: CharMap a
empty = Empty

-- | Check if a map is empty. \(O(1)\).
isEmpty :: CharMap a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | Create a singleton map. \(O(1)\).
singleton :: Char -> a -> CharMap a
singleton k v = Node B Empty k v Empty

-- | Check if a map is a singleton, returning the key-value pair if it is.
-- \(O(1)\).
isSingleton :: CharMap a -> Maybe (Char, a)
isSingleton (Node _ Empty c a Empty) = Just (c, a)
isSingleton (Node _ _ _ _ _) = Nothing 
isSingleton Empty = Nothing 

-- | Enforce the Red-Black invariants. \(O(1)\).
--
-- Eliminates red-red violations by pulling the red colour up.
-- all we do is convert black - red -red to a black with two red children locally.
balance :: Colour -> CharMap a -> Char -> a -> CharMap a -> CharMap a
balance B (Node R (Node R a xk xv b) yk yv c) zk zv d = 
    -- case 1: B (R R)  _ 
    Node R (Node B a xk xv b ) yk yv (Node B c zk zv d) 
balance B (Node R a xk xv (Node R b yk yv c)) zk zv d = 
    -- case 2: B (B _) R 
    Node R (Node B a xk xv b ) yk yv (Node B c zk zv d) 
balance B a xk xv (Node R (Node R b yk yv c) zk zv d) = 
    -- case 3: B _ (R R)  
    Node R (Node B a xk xv b ) yk yv (Node B c zk zv d) 
balance B a xk xv (Node R b yk yv (Node R c zk zv d)) = 
    -- case 4: B _ ((R _) R)  
    Node R (Node B a xk xv b ) yk yv (Node B c zk zv d) 
balance col l k v r = Node col l k v r

-- | Insert with a combining function for collisions. \(O(\log{}(A))\).
--
-- The combining function is applied as @f new old@.
--
-- >>> let m1 = insert 'a' 1 empty
-- >>> let m2 = insertWith (+) 'a' 2 m1
-- >>> lookup 'a' m2
-- Just 3
insertWith :: (a -> a -> a) -> Char -> a -> CharMap a -> CharMap a
insertWith f xk xv m = 
    let 
        ins Empty = Node R Empty xk xv Empty 
        ins c'@(Node c l yk yv r) = 
            if xk < yk then balance c (ins l) yk yv r
            else if xk > yk then balance c l yk yv (ins r)
            else -- duplicate 
                balance c l yk (f xv yv) r
        Node _ l yk yv r = ins m 
    in Node B l yk yv r

-- | Insert a key-value pair into the map, overwriting values if the key already
-- exists. \(O(\log{}(A))\).
insert :: Char -> a -> CharMap a -> CharMap a
insert = insertWith (\new _ -> new)

-- | Look up the value associated with a key. \(O(\log{}(A))\).
lookup :: Char -> CharMap a -> Maybe a
lookup _ Empty = Nothing
lookup yk (Node _ l xk xv r) =
    -- just recurse down 
    if xk < yk then lookup yk l
    else if xk < yk then lookup yk r
    else Just xv

-- | /Strict/ left fold over key-value pairs. \(O(A)\).
foldlWithKey' :: (b -> Char -> a -> b) -> b -> CharMap a -> b
foldlWithKey' _ b Empty = b
foldlWithKey' f acc (Node _ l xk xv r) = 
    -- classic tree fold. use ! for stricness
    let 
        !ls = foldlWithKey' f acc l 
        !node = f ls xk xv 
        !rs =  foldlWithKey' f node r 
    in rs

-- | Build a map from a list of key-value pairs. \(O(N\log{}(A))\). Retains the
-- latest values on key collisions.
--
-- >>> toAscList (fromList [('b', 2), ('a', 1)])
-- [('a',1),('b',2)]
fromList :: [(Char, a)] -> CharMap a
-- the acc is of value while each element is (k, v)
fromList = foldl' (\m (k, v) -> insert k v m) empty 

-- | Convert the map to an association list in ascending order of keys.
-- \(O(A)\). Avoids \(O(A^2)\) concatenations by building the list
-- right-to-left.
toAscList :: CharMap a -> [(Char, a)]
toAscList = _toAscList 
