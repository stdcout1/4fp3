module Main where

import Spec.CharMap
import Spec.Huffman
import Spec.PriorityQueue
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "A4"
      [ priorityQueueTests,
        charMapTests,
        huffmanTests
      ]
