module Huffman where

import CharMap qualified as M
import Data.Foldable (foldl')
import Data.Function (on)
import PriorityQueue qualified as PQ

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
        -- the min PQ will prio shortest tree until 
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
        popper pQ
    

-- | Generate a Huffman code table from a Huffman tree. \(O(A\log{}(A))\).
codeTable :: Tree -> Table
codeTable = _codeTable -- Hint: Use a /difference list/ to track the path to each leaf

-- | Encode a string using the provided code table. \(O(N\log{}(A))\).
encodeMessage :: Table -> String -> HCode
encodeMessage = _encodeMessage

-- | Decode a stream of bits using the Huffman tree. \(O(B)\), where B is the
-- length of the stream.
decodeMessage :: Tree -> HCode -> String
decodeMessage = _decodeMessage -- Hint: Traverse the tree according to the bits, emitting characters at all leaves
