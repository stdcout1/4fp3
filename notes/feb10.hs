module Feb10 where

import qualified Data.Map as Map

-- Another: the State monad
-- A representation of a computation of values of type 'a'
-- in a state 's'
-- "world-passing style", except the state is *local*
data State s a = State { trans :: s -> (s, a) }

-- implement it!!
instance Functor (State s) where
  fmap f (State st) = State $ \s -> let (s', a) = st s in (s' , f a)

instance Applicative (State s) where
  pure x = State $ \s -> (s, x)
  (State stf) <*> (State sta) = State $
    \s -> let (s', f) = stf s in
          let (s'', a) = sta s' in
          (s'', f a)

instance Monad (State s) where
  (State st) >>= k = State $ \ s ->
    let (s', a) = st s in
    trans (k a) s'

runST :: State s a -> s -> a
runST (State st) s = snd $ st s

get :: State s a -> State s s
get (State st) = State $ \s -> let (s', _) = st s in (s', s')

-- Feb10 stuff starts here

------------------
-- identity Monad

newtype Ident a = I {unI :: a}

instance Functor Ident where
  fmap f x = I $ f $ unI x

instance Applicative Ident where
  pure = I
  f <*> x = I $ (unI f) (unI x)

instance Monad Ident where
  x >>= k = k $ unI x

-- Also useful: Reader and Error
-- newtype Reader ctx a = Reader { unR :: ctx -> a }
-- data Error e a = Err e | Success a

-- an exercise in Monadic programming

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving Show

-- test data
tt :: Tree String
tt = Node "foo" (Node "bar" Nil Nil)
                (Node "foo" (Node "thing" Nil Nil) (Node "bar" Nil Nil))
-- replace every node's data with an Int, where the
-- Int represents every new thing we encounter
-- we use this state that we made
numTree :: Ord a => Tree a -> Tree Int
numTree t = runST (numberTree t) (Map.empty)

type Table a = Map.Map a Int

-- actually do the 'hard' work (tree traversal)
numberTree :: Ord a => Tree a -> State (Table a) (Tree Int)
numberTree Nil = pure Nil
numberTree (Node x l r) =
  do
    -- infix traversal
    y <- numberNode x
    l' <- numberTree l
    r' <- numberTree r
    pure (Node y l' r')

-- the work that needs the state is here
numberNode :: Ord a => a -> State (Table a) Int
numberNode = State . nNode

nNode :: Ord a => a -> (Table a -> (Table a,Int))
nNode x tab =
  case (Map.lookup x tab) of
    (Just y) -> (tab, y)
    Nothing  -> let l = Map.size tab in
                (Map.insert x l tab , l)

----------------------------
-- Back to DSLs

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  sub :: e -> e -> e
  mul :: e -> e -> e

-- Just a wrapper on Integer
-- newtype R = R Integer
-- unR :: R -> Integer
-- unR (R a) = a
-- completely identical to 
newtype R = R {unR :: Integer}

{-
instance Expr R where
  lit i = R i
  add (R a) (R b) = R $ a + b
  sub (R a) (R b) = R $ a - b
  mul (R a) (R b) = R $ a * b

-- but also do it by moving the matching to the right
-}

-- instance Expr R where
--   lit = R
--   add a b = R $ unR(a) + unR(b)
--   etc..

-- by calling unR ee we do this evalution on this... evetually resovling to an integer.
ee :: Expr e => e
ee = add (lit 1) (sub (lit 5) (lit 3))

-- factor out the commonalities:
liftR2 :: (Integer -> Integer -> Integer) -> R -> R -> R
liftR2 f ra rb = R $ f (unR ra) (unR rb)

-- comment out above instance and redo:
instance Expr R where
  lit = R
  add = liftR2 (+)
  sub = liftR2 (-)
  mul = liftR2 (*)

-- printing!
newtype T = T { unT :: String }

-- learning from our lesson:
inbetween :: String -> T -> T -> T
inbetween s a b = T $ "(" ++ unT a ++ s ++ unT b ++ ")"

instance Expr T where
  lit i = T $ show i
  add = inbetween " + "
  sub = inbetween " - "
  mul = inbetween " * "

up1 :: C -> C -> C
up1 a b = C $ (unC a) + (unC b) + 1

-- counting nodes
newtype C = C Integer
unC :: C -> Integer
unC (C a) = a

instance Expr C where
  lit _ = C 1
  add = up1
  sub = up1
  mul = up1

{-
-- do it again, but *typed*
-- add BoolExpr
-- add IfExpr
-- add Lambda

-}

