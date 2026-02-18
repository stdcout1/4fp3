import Data.Text.Internal.Read (IParser(P))
import Control.Arrow (Arrow(first))
-- first we start with a case study of parsing... 

-- math expressions
type Var = Char
data Expr = Lit Integer | Var Var | BOp BOps Expr Expr
  deriving Show
data BOps = Add | Sub | Mul | Div | Mod
  deriving Show

--we parse them as a list of sucesses and leftovers 
--parsing can return mulltiple sucessful parses 
--that may be concsume a certai amount of input
type Parse a b = [a] -> [(b, [a])]

-- take and input and fail the parse  
none :: Parse a b
none _ = []

-- take the input a parse sucessfully no matter what, 
-- dont read input
succeed :: b -> Parse a b
succeed val = \input -> [(val, input)]

-- recognize a single token. (consume a token and see if is t)
token :: Eq a => a -> Parse a a
token _ [] = []
token t (x : xs)
    | t == x = [(x, xs)]
    | otherwise = []

-- check if a token a specific property 
spot :: (a -> Bool) -> Parse a a
spot f [] = []
spot f (x : xs)
    | f x = [(x, xs)]
    | otherwise = []


-- simple parsers from above
lparens, rparens, dig :: Parse Char Char
lparens = token '('
rparens = token ')'
dig = spot isDigit

-- instead of importing ... 
isDigit :: Char -> Bool
-- isDigit c = c `elem` ['0'..'9']
-- faster:
isDigit c = c >= '0' && c <= '9'

-- combining parsers 
-- this allows us to parse one then another then another 
infixr 5 >*>
(>*>) :: Parse a b -> Parse a c -> Parse a (b, c)
(>*>) p1 p2 input =
    -- we parse p1, then use the rest of p1 to parse using p2 then return the rest of that
    [((b, c), rest') | (b, rest) <- p1 input, (c, rest') <- p2 rest ]


-- either succeeds
alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 t = p1 t ++ p2 t

-- ahhh this is kind of going under the thing like a map
build :: Parse a b -> (b -> c) -> Parse a c
build p f input = [(c, rest) | (b, rest) <- p input, let c = f b  ]
-- or 
-- build p f input = [(f b, rest) | (b, rest) <- p input  ]
-- or with map ...
build' :: Parse a b -> (b -> c) -> Parse a c
build' p f input = map (first f) (p input)
-- its actually
-- build' p f input = map (\(b, as) -> (f b, as) ) (p input) 

-- kleene star...
many :: Parse a b -> Parse a [b]
-- many p = (p >*> many p) `build` (uncurry (:))
--       this is 
--       (b, [b])
--       so we want 
--       ((b, [b]) -> (b : b))
--       snd wwould throw the b 
--       we need to add the empty match as * matcches no occurances aswell
many p = succeed [] `alt` ((p >*> many p) `build` uncurry (:))

--alternate way
atleastone :: Parse a b -> Parse a [b]
atleastone p = (p >*> many p) `build` (uncurry (:))

many' :: Parse a b -> Parse a [b]
many' p = succeed [] `alt` atleastone p


-- combining parsers
infixr 5 -*>
infixr 5 >*-

-- 'then' but ignore lhs
(-*>) :: Parse a b -> Parse a c -> Parse a c
(-*>) p1 p2 inp =
  [ (c, rest') | (_, rest) <- p1 inp, (c, rest') <- p2 rest ]

-- 'then' but ignore rhs
(>*-) :: Parse a b -> Parse a c -> Parse a b
(>*-) p1 p2 =
  -- [ (b, rest') | (b, rest) <- p1 inp, (_, rest') <- p2 rest ]
  (p1 >*> p2) `build` fst

build2 :: Parse a (b , c) -> (b -> c -> d) -> Parse a d
build2 p f = p `build` (uncurry f)

------------------------------------------------

