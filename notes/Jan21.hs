(>.>) :: (a -> b) -> (b -> c) -> a -> c
f >.> g = \x -> g (f x)

whitespace :: String
whitespace = " \t\n"

member :: Eq a => a -> [a] -> Bool 
member x [] = False 
member x (y : ys) | x == y = True 
                  | otherwise = member x ys
--dropLeadingSpace
dropLeadingSpace :: String -> String 
dropLeadingSpace = dropWhile (`member` whitespace)

--getWord 
getWord :: String -> String 
getWord = takeWhile (not . (`member` whitespace))

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \ a b -> f (a, b)
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(a, b) -> f a b


--regex 
type RegExp = String -> Bool -- we do this to prevent us from writing String -> Bool

epsilon :: RegExp
epsilon = (== "") -- is it empty not " " 

char :: Char -> RegExp
char c = (== [c]) 

(|||) :: RegExp -> RegExp -> RegExp
r1 ||| r2 = \s -> r1 s || r2 s

splits :: String -> [(String, String)]
splits "" = [("", "")]
splits l@(x : xs) = ([], l) :
  map (\(a , b) -> ( x : a, b)) (splits xs)

(<**>) :: RegExp -> RegExp -> RegExp
r1 <**> r2 = \s -> or [r1 t && r2 u | (t, u) <- splits s]

star :: RegExp -> RegExp
star r = epsilon ||| (r <**> star r)

