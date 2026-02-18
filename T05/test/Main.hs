module Main (main) where

import Prelude hiding (reverse)

import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty

reverse :: [a] -> [a]
reverse = foldr (:) []

prop_reverse_reverse :: [Int] -> Bool 
prop_reverse_reverse xs = reverse (reverse xs) == xs

prop_reverse_concat :: [Int] -> [Int] -> Bool 
prop_reverse_concat xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs


main :: IO ()
main = defaultMain $ 
    testGroup "T05" 
    [
        testProperty "reverse reverse" prop_reverse_reverse,
        testProperty "reverse concat" prop_reverse_concat

    ]
