module A6.Para
  ( -- * Paramorphisms
    RAlgebra,
    para,

    -- * Lists
    deleteMin,
    findMin,
    wordCount,
  )
where

import A6.CataAna
import A6.Fix (Fix (..))

--------------------------------------------------------------------------------
-- PARAMORPHISMS
--------------------------------------------------------------------------------

-- some payload? bifunctor instead of this weirdness
-- f a b -> a
-- functor is just applying to a 
type RAlgebra f a = f (Fix f, a) -> a

para :: (Functor f) => RAlgebra f a -> Fix f -> a
para alg = alg . fmap (\x -> (x, para alg x)) . out

--------------------------------------------------------------------------------
-- LIST OPERATIONS
--------------------------------------------------------------------------------

deleteMinAlg :: (Ord a) => RAlgebra (ListF a) (Maybe (a, List a))
-- ListF a (Fix f, a) -> (Maybe (a, List a))
-- i think
deleteMinAlg NilF = Nothing 
-- last element...
deleteMinAlg (ConsF x (xs, Nothing)) = Just (x, xs)
-- we got a min of m so we need to check if x is lower 
deleteMinAlg (ConsF x (xs, Just (m, tl))) = 
    if x <= m then 
        -- x is lower 
        Just (x, xs)
    else 
        Just (m, In $ ConsF x tl)

-- | Delete the /first occurrence/ of the minimum element in a list.
deleteMin :: (Ord a) => List a -> Maybe (a, List a)
deleteMin = para deleteMinAlg

findMinAlg :: (Ord a) => RAlgebra (ListF a) (Maybe (List a, a, List a))
findMinAlg NilF = Nothing
findMinAlg (ConsF x (xs, Nothing)) = Just (In NilF, x, xs)
findMinAlg (ConsF x (xs, Just (before, m, after))) = 
-- we got a min of m sandwiched, we need to check if toss m into left or x in to right 
    if x <= m then 
        -- x is lower so we toss m in after. (which is already the case in xs)
        Just (In NilF, x, xs)
    else 
        -- idk why we can add to front but it works :3
        Just (In $ ConsF x before , m, after)

-- | Find the minimum element in a list, along with the elements to the left and
-- right of the /first occurrence/ of the minimum element.
findMin :: (Ord a) => Fix (ListF a) -> Maybe (List a, a, List a)
findMin = para findMinAlg

--------------------------------------------------------------------------------
-- WORD COUNTING
--------------------------------------------------------------------------------

wordCountAlg :: RAlgebra (ListF Char) Int
-- ListF char (Fix f, char) -> Int
wordCountAlg NilF = 0 
wordCountAlg (ConsF x (In NilF, wc) ) = if not (isSpace x) then wc + 1 else wc
wordCountAlg (ConsF x (In (ConsF next _), wc)) = if not (isSpace x) && isSpace next then wc + 1 else wc

-- | Count the number of words in a string.
wordCount :: List Char -> Int
wordCount = para wordCountAlg

-- | A character is whitespace if it is a space, tab, or newline.
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace '\n' = True
isSpace _ = False
