reverse_acc :: [a] -> [a] -> [a]
reverse_acc [] ys = ys
reverse_acc (x : xs) ys = reverse_acc xs (x : ys)


sum' :: Num a => [a] ->a 
sum'= foldr (+) 5


zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [] (_ : _) = []
zip' (_ : _) [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys
