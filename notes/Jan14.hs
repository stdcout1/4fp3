map' :: (a -> b) -> [a] -> [b] 
map' _ [] = []
map' f (x : xs) = f x : map' f xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- zipwith zipwith f 
-- ghci> :t \f -> zipWith' (zipWith' f)
-- \f -> zipWith' (zipWith' f)
--  :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
--  looks weird but makes sense 

-- insertion sort 
-- ins x into xs where xs is already sorted 
ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x xs@(y : ys) | x > y = y : ins x ys 
                  | otherwise = x : (y : ys) 
                  -- or equiv : x : xs

-- now the sort 
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x : xs) = ins x (iSort xs)

-- qSort 
qSort :: Ord a => [a] -> [a] 
qSort [] = [] 
qSort (x : xs) = 
    let (front, back) = partition (x >) xs
    in qSort front ++ (x : qSort back)


-- a choice between things 
data Either' a b = Left' a | Right' b 

--could make bool like this 
data Boolean = Either' () ()

-- either :: Either' a b -> c
-- either (Left' a) = 
-- either (Right' b) = 
-- shit im stuck... 
either :: (a -> c) -> (b -> c) -> Either' a b -> c
either f _ (Left' a) = f a
either _ g (Right' b) = g b 

