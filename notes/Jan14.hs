import Data.List (partition, sortBy)
import Data.Tuple (swap)
import Data.Function (on)

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

-- required imports for below
abs' :: Integer -> Integer
abs' x = if x < 0 then -x else x

abs'' :: Integer -> Integer
abs'' x
  | x < 0     = -x
  | otherwise = x

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

beat, lose :: RPS -> RPS

beat Rock = Scissors
beat Paper = Rock
beat Scissors = Paper

lose Rock = Paper
lose Paper = Scissors
lose Scissors = Rock

data Result = Win | Lose | Draw deriving (Eq, Show)

-- outcome of P1 playing against P2, outcome for P1
outcome :: RPS {- player 1-} -> RPS {- Player 2 -} -> Result
outcome m1 m2
  | beat m1 == m2 = Win
  | lose m1 == m2 = Lose
  | otherwise     = Draw

-- this is just as good, given that *lose* is invertible
outcome' :: RPS {- player 1-} -> RPS {- Player 2 -} -> Result
outcome' m1 m2
  | beat m1 == m2 = Win
  | beat m2 == m1 = Lose
  | otherwise     = Draw

swap :: (a, b) -> (b, a)
swap (x , y) = (y, x)

swap' :: a -> (b -> (b , a))
swap' x y = (y, x)

data Maybe' a = Just' a | Nothing'

-- eliminator for Maybe'
maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' _ f (Just' a) = f a
maybe' b _ Nothing'  = b

data List' a = Nil | Cons a (List' a)
--   [ a ]     []    (a : [a])

--



-- list of moves    p1  p2
type Tournament = [(RPS,RPS)]

-- -1 for Lose, 0 for Draw, 1 for Win
score :: Result -> Integer
score Lose = -1
score Win  = 1
score Draw = 0

-- from the point of view of the first player
tournamentResult :: Tournament -> Integer
tournamentResult [] = 0
tournamentResult ((m1, m2) : ms) = score (outcome m1 m2) + tournamentResult ms

-- given some previous moves, compute the next move
-- convention: most recent move is at head
type Strategy = [RPS] -> RPS

{-
-- 3 constant strategies
-- cyclic strategy
-- random????
-- repeat latest
-- winner against most often used
-- ???
-}
sRock :: [RPS] -> RPS
sRock _ = Rock

sCyclic :: [RPS] -> RPS
sCyclic p =
  let l = length p in
  case l `mod` 3 of
   0 -> Scissors
   1 -> Paper
   2 -> Rock
   _ -> error "impossible"

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x : []) = Just x
lastMaybe (_ : xs) = lastMaybe xs

sLatest :: [RPS] -> RPS
sLatest l = maybe Paper id (lastMaybe l)

sMostOften :: [RPS] -> RPS
sMostOften [] = Rock
sMostOften l@(_ : _) = head (head q)
  where
    r :: [RPS]
    r = filter (== Rock) l
    p = filter (== Paper) l
    s = filter (== Scissors) l
    q :: [[RPS]]
    -- q = sortBy (\x y -> compare (length y) (length x)) [r, p , s]
    q = sortBy ((flip compare ) `on` length) [r, p, s]

{-
 Why is I/O hard in Haskell?
 - functions in Haskell are pure, i.e. no side effects, i.e. will always
   return the same answer with the same inputs. ALWAYS.

 - predefined functions:
 putStr :: String -> IO ()
 putStrLn :: String -> IO ()
 print :: Show a => a -> IO ()
 -- defined as putStrLn . show
 return :: a -> IO a
-}

-- can the unit type be used on its own?
dummy :: () -> String
dummy () = "foo"

-- can create our own Unit type
data Unit = Unit

helloWorld :: IO ()
helloWorld = print "Hello World!"

-- do notation: sequencing actions (later: more than just IO)

pStrLn :: String -> IO ()
pStrLn s =
  do putStr s
     putStr "\n"

-- We can also read
-- getLine :: IO String

echo :: IO ()
echo = do line <- getLine
          pStrLn line


-- Can also write

echo' :: IO ()
echo' = do { line <- getLine ; pStrLn line }

copy :: IO ()
copy =
  do line <- getLine
     putStrLn line
     copy

copyN :: Integer -> IO ()
copyN i =
  if (i <= 0) then
    return ()
  else
    do
      line <- getLine
      putStrLn line
      copyN (i-1)
charToMove :: Char -> RPS
charToMove 'r' = Rock
charToMove 'R' = Rock
charToMove 'p' = Paper
charToMove 'P' = Paper
charToMove 's' = Scissors
charToMove 'S' = Scissors
charToMove _   = error "invalid"

play :: Strategy -> IO ()
play strat = playInteractive strat []

playInteractive :: Strategy -> Tournament -> IO ()
playInteractive s t =
  do
    c <- getChar
    if not (c `elem` "rpsRPS") then
      print $ tournamentResult t
    else
      do
        let move = s (map fst t)
        print ("player played "++[c]++" and computer played "++show move++".")
        playInteractive s ((charToMove c , move) : t)
  -- get a character (getChar)
  -- if it's not valid, showResults t
  -- otherwise
  --   compute next computer move using strategy on your moves
  --   print what each played
  --   convert the character to a move
  --   play that move recursively

