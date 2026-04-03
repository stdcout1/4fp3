{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Avoid lambda" -}

module A5.Compile
  ( -- * Expressions
    Expr (..),
    Var (..),

    -- * Computations
    UnOp (..),
    BinOp (..),
    Step (..),
    Prog (..),
    FreshM (..),
    freshName,

    interpretProg,
  )
where

import A5.Language

--------------------------------------------------------------------------------
-- Expressions

-- | Variables.
data Var
  = -- | User-written variables.
    User String
  | -- | Machine generated names.
    Machine Int
  deriving (Show, Ord, Eq)

data Expr
  = Var Var
  | Int Int
  | Bool Bool
  | Array [Expr]
  deriving (Show, Ord, Eq)

data UnOp
  = Not
  | Length
  deriving (Show, Ord, Eq)

data BinOp
  = Add
  | Sub
  | Mul
  | And
  | Or
  | Xor
  | Leq
  | Lt
  | Get
  deriving (Show, Ord, Eq)

data TernOp = Set
  deriving (Show, Ord, Eq)

data Step
  = UnOp UnOp Expr
  | BinOp BinOp Expr Expr
  | TernOp TernOp Expr Expr Expr
  | ForUp Expr Expr Expr Prog
  | ForDown Expr Expr Expr Prog
  deriving (Show, Ord, Eq)

data Prog
  = Step Step
  | Let Step Prog
  | If Expr Prog Prog
  | Ret Expr
  | Crash String
  deriving (Show, Ord, Eq)

newtype FreshM a = FreshM {runFreshM :: Int -> a}
  deriving (Functor, Applicative, Monad)

-- | Generate a fresh name.
freshName :: (Expr -> FreshM a) -> FreshM a
freshName k = FreshM \i ->
  runFreshM (k (Var (Machine i))) $! i + 1

bin :: BinOp -> Expr -> Expr -> FreshM Prog
bin op l r = pure $ Step (BinOp op l r)

un :: UnOp -> Expr -> FreshM Prog
un op e = pure $ Step (UnOp op e)

instance Lang Expr (FreshM Prog) where
  -- ez pz
  int = Int
  add = bin Add
  sub = bin Sub
  mul = bin Mul

  bool = Bool
  not = un Not
  and = bin And
  or = bin Or
  xor = bin Xor
  leq = bin Leq
  lt = bin Lt

  array = Array
  getArray = bin Get
  setArray a pos i = pure $ Step (TernOp Set a pos i)
  lenArray = un Length

  let_ prog body = do
    p <- prog
    case p of
      -- what the hell...
      Step call -> Let call <$> freshName body
      -- flatten the monad
      -- we can do Let s <$> let_ (pure p') body but i like do notation and we have monad
      -- so why confused ourselves with fmap
      Let s p' -> do
        -- do the body under p' and return a prog 
        -- then excute the step under our new body prog
        p'' <- let_ (pure p') body 
        pure $ Let s p''
        
      -- if with let binding in each
      If b l r -> if_ b (let_ (return l) body) (let_ (return r) body) 
      -- do the body
      Ret e -> body e
      -- crashie
      Crash msg -> pure $ Crash msg

  ret x = pure $ Ret x 

  forUp lo hi acc body =
    -- Step that has a forUp lo hi acc. but we need to convert
    let x = freshName \ivar -> freshName \accvar -> body ivar accvar
     in Step . ForUp lo hi acc <$> x

  forDown hi lo acc body = 
    -- setup a function for prog -> prog (that converts out forDown to ForDown). then go under FreshM prog and apply that function.
    Step . ForDown hi lo acc <$> freshName \i -> freshName \acc' -> body i acc'

  if_ b prog1 prog2 = do
    p1 <- prog1
    p2 <- prog2
    pure $ If b p1 p2

  crash msg = pure $ Crash msg

-- extent our look function witha new val
bind :: Int -> val -> (Var -> val) -> (Var -> val)
-- delegate incrementation to caller
bind i v look (Machine n) = if n == i then v else look (Machine n)
bind _ _ look var = look var

interpretExpr :: (Lang val cmp) => Expr -> (Var -> val) -> val
interpretExpr (Var var) look = look var
interpretExpr (Int i) _ = int i
interpretExpr (Bool b) _ = bool b
interpretExpr (Array a) f = array $ fmap (\i -> interpretExpr i f) a 

-- '<,'>s/\(\w.*\) =.*/interpretStep (BinOp \u\1 l r) look = \1 (interpretExpr l look) (interpretExpr r look)
interpretStep :: (Lang val cmp) => Step -> (Var -> val) -> cmp
interpretStep (UnOp Not e) look = A5.Language.not $ interpretExpr e look
interpretStep (UnOp Length e) look = lenArray $ interpretExpr e look
interpretStep (BinOp Add l r) look = add (interpretExpr l look) (interpretExpr r look)
interpretStep (BinOp Sub l r) look = sub (interpretExpr l look) (interpretExpr r look)
interpretStep (BinOp Mul l r) look = mul (interpretExpr l look) (interpretExpr r look)
interpretStep (BinOp And l r) look = A5.Language.and (interpretExpr l look) (interpretExpr r look)
interpretStep (BinOp Or l r) look = A5.Language.or (interpretExpr l look) (interpretExpr r look)
interpretStep (BinOp Xor l r) look = xor (interpretExpr l look) (interpretExpr r look)
interpretStep (BinOp Leq l r) look = leq (interpretExpr l look) (interpretExpr r look)
interpretStep (BinOp Lt l r) look = lt (interpretExpr l look) (interpretExpr r look)
interpretStep (BinOp Get l r) look = getArray (interpretExpr l look) (interpretExpr r look)
interpretStep (TernOp Set arr i v) look = setArray (interpretExpr arr look) (interpretExpr i look) (interpretExpr v look) 
interpretStep step look = interpretStep' step 0 look
-- we need to count how deep we are...
-- very messy :(
interpretStep' :: (Lang val cmp) => Step -> Int -> (Var -> val) -> cmp
interpretStep' (ForUp low hi acc body) i look =
  forUp (interpretExpr low look) (interpretExpr hi look) (interpretExpr acc look)
      (\v1 v2 -> interpretProg' body (i+2) (bind (i+1) v2 (bind i v1 look)))
interpretStep' (ForDown low hi acc body) i look =
  forDown (interpretExpr low look) (interpretExpr hi look) (interpretExpr acc look)
      (\v1 v2 -> interpretProg' body (i+2) (bind (i+1) v2 (bind i v1 look)))
interpretStep' step _ look = interpretStep step look




interpretProg :: (Lang val cmp) => Prog -> (Var -> val) -> cmp
interpretProg prog look = interpretProg' prog 0 look

-- its messy buts its the only way i could get the lookup to return the correct varible... 
-- the test case would look up the wrong varible so i had to count what varible we are at 
interpretProg' :: (Lang val cmp) => Prog -> Int -> (Var -> val) -> cmp
interpretProg' (Step step) i look = interpretStep' step i look
interpretProg' (Let step prog) i look =
  let_ (interpretStep' step i look)
    -- incremenet the machine by 1
    (\v -> interpretProg' prog (i + 1) (bind i v look))
interpretProg' (If b thb elb) i look =
  if_ (interpretExpr b look) (interpretProg' thb i look) (interpretProg' elb i look)
interpretProg' (Ret expr) _ look = ret (interpretExpr expr look)
interpretProg' (Crash msg) _ _ = crash msg


