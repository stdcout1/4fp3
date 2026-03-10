module Huffman where

import CharMap qualified as M
import Data.Foldable (foldl')
import Data.Function (on)
import PriorityQueue qualified as PQ
import Data.Maybe (fromMaybe)

-- | Huffman tree. The 'Int' parameter tracks the aggregate frequency weight.
data Tree
  = Leaf !Char !Int
  | Node !Int !Tree !Tree
  deriving (Eq, Show)

-- | Binary path discriminator. L = left, R = right.
data Bit
  = L
  | R
  deriving (Eq, Show)

-- | A sequence of bits representing compressed data.
type HCode = [Bit]

-- | Dictionary mapping characters to their Huffman codes.
type Table = M.CharMap HCode

-- | Dictionary tracking character occurrence counts.
type CharFrequencies = M.CharMap Int

-- | Extract the frequency weight of a subtree. \(O(1)\).
weight :: Tree -> Int
weight (Leaf _ w) = w
weight (Node w _ _) = w

instance Ord Tree where
  compare = compare `on` weight

-- | Count character occurrences using a strict left fold. \(O(N\log{}(A))\).
frequencies :: String -> CharFrequencies
frequencies = foldl (\m c -> M.insertWith (+) c 1 m ) M.empty
-- take the old value and plus one
-- or set as one

-- | Construct a Huffman tree from an input string. \(O(N\log{}(A))\).
buildTree :: String -> Maybe Tree
buildTree = buildHuffman . frequencies

-- | Greedily construct a Huffman tree via Priority Queue reduction on a
-- frequency table. \(O(A\log{}(A))\). Handles the A=1 edge case by inserting a
-- dummy '\0' leaf.
buildHuffman :: CharFrequencies -> Maybe Tree
buildHuffman f =
    let
        -- setup a pq with nodes as a number and a leag tagged char
        pQ = M.foldlWithKey' (\pq c n -> PQ.insert (Leaf c n) pq) PQ.empty f
        -- then we pop from it until there is nothing and add it as a branch in our tree 
        -- we made a node with the children and add it 
        -- the min PQ will prio shortest tree (which will be the incomplete tree) until 
        -- we have one tree left which is exactly the huffman 
        -- tree!
        popper pq = do
            (l, pq') <-PQ.popMin pq
            case PQ.popMin pq' of
                Nothing -> Just l
                Just (l', pq'') ->
                    let w = weight l + weight l'
                    in popper (PQ.insert (Node w l l') pq'')
    in
        case PQ.popMin pQ of
        -- handle A = 1 edge case
            Nothing -> Nothing
            Just (t, rest) ->
                if PQ.isEmpty rest then
                    Just (Node (weight t) t (Leaf '\0' 0)) -- we add a dummy node as spcified in the haddocs
                else
                    popper pQ


-- | Generate a Huffman code table from a Huffman tree. \(O(A\log{}(A))\).
codeTable :: Tree -> Table
codeTable tr =
    let
    -- lets accumulate a table 
    -- move through the tree 
    -- the hCode -> HCode is a function representing the previous path. 
    -- so like f L -> L ++ LRLRL if the path so far is LRLRL
    tree :: (HCode -> HCode) -> Tree -> Table -> Table
    tree f (Leaf c _) table =
        M.insert c (f []) table
    tree f (Node _ l r) table =
        -- man this looks weird but we are just `adding` R or L to the path and recusring down. 
        -- maybe instead of HCode -> HCode we could do just HCode ? but i couldnt get that to wrk withouth O(A^2)... 
        -- anyways this is from here 
        -- https://wiki.haskell.org/Difference_list
        --
        let table' = tree (f . (L:)) l table
        in tree (f . (R:) ) r table'
    in
    tree id tr M.empty


-- | Encode a string using the provided code table. \(O(N\log{}(A))\).
encodeMessage :: Table -> String -> HCode
encodeMessage tbl =
    -- its telling me to use maybe but idk how that works 
    -- but this basically lookup. if any of them are empty return Nothing and thus []
    -- if not have a list of values that i can concat
    -- look up is log A and we do that once over the string so O(a log a)
    -- but concat has to add up bits which could be B long... >> N!
    concat . fromMaybe [] . mapM (`M.lookup` tbl)
-- | Decode a stream of bits using the Huffman tree. \(O(B)\), where B is the
-- length of the stream.
decodeMessage :: Tree -> HCode -> String
decodeMessage tr = 
    -- yea lets follow the hint. follow the traversal until we hit a leaf. then emit the character 
    let
        -- if we are at a node and we recive a L instruction traaverse left
        decodeChar (Node _ l _) (L : rest) = decodeChar l rest
        -- if we are at a node and we recive a R instruction traaverse right
        decodeChar (Node _ _ r) (R : rest) = decodeChar r rest
        -- if we r at a leaf add it to our list. and move on to the next bit with a clean slate
        decodeChar (Leaf c _) rest = c : decodeChar tr rest
        -- once we at the end stop
        decodeChar _ [] = []
    in
    decodeChar tr
