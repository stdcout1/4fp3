module Main where

import Data.Maybe (fromMaybe)
import Huffman (buildTree, codeTable, encodeMessage)
import Text.Printf (printf)

main :: IO ()
main = do
  input <- readFile "dna.txt"
  let tree = fromMaybe (error "Error: Empty input") (buildTree input)
      table = codeTable tree
      compressed = encodeMessage table input
      
      uncompressedBits = length input * 8
      compressedBits   = length compressed
      ratio            = fromIntegral compressedBits / fromIntegral uncompressedBits :: Double

  printf "Uncompressed: %d bits\n" uncompressedBits
  printf "Compressed:   %d bits\n" compressedBits
  printf "Savings:      %.2f%%\n" ((1 - ratio) * 100)
