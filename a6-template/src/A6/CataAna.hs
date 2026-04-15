module A6.CataAna
  ( -- * Cata- and Anamorphisms
    Algebra,
    cata,
    Coalgebra,
    ana,

    -- * Lists
    ListF (..),
    List,
    toList,
    fromList,

    -- * Expression language
    ExprF (..),
    freeVars,
  )
where

import A6.Fix (Fix (..))
import Data.Set qualified as S

--------------------------------------------------------------------------------
-- CATAMORPHISMS
--------------------------------------------------------------------------------

type Algebra f a = f a -> a

-- | Catamorphism: "fold"
cata :: (Functor f) => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

--------------------------------------------------------------------------------
-- ANAMORPHISMS
--------------------------------------------------------------------------------

type Coalgebra f a = a -> f a

-- | Anamorphism: "unfold"
ana :: (Functor f) => Coalgebra f a -> a -> Fix f
ana coalg = In . fmap (ana coalg) . coalg

--------------------------------------------------------------------------------
-- LISTS
--------------------------------------------------------------------------------

data ListF a r = NilF | ConsF a r
  deriving (Functor)

type List a = Fix (ListF a)

toList :: List a -> [a]
toList = cata alg
  where
    alg :: Algebra (ListF a) [a]
    alg NilF = []
    alg (ConsF a r) = a : r

fromList :: [a] -> List a
fromList = ana coalg
  where
    coalg :: Coalgebra (ListF a) [a]
    coalg (x : xs) = ConsF x xs
    coalg [] = NilF

--------------------------------------------------------------------------------
-- EXPRESSION LANGUAGE
--------------------------------------------------------------------------------

data ExprF r
  = IntF Int
  | AddF r r
  | MulF r r
  | VarF String
  | LetF String r r
  | LamF String r
  | AppF r r
  deriving (Functor)

type Expr = Fix ExprF

freeVars :: Expr -> S.Set String
freeVars = cata alg
  where
    alg :: Algebra ExprF (S.Set String)
    alg (IntF _) = S.empty
    alg (AddF l r) = l `S.union` r
    alg (MulF l r) = l `S.union` r
    alg (VarF name) = S.singleton name
    -- let binder = body in expression 
    -- the binder is not free in body nor expression 
    alg (LetF binder body expression) = (S.delete binder body) `S.union` (S.delete binder expression) 
    -- the binder var is no longer free
    alg (LamF binder body) = S.delete binder body
    -- after sub the var becomes free 
    alg (AppF f arg)  = f `S.union` arg 
-- SHORT ANSWER (1-2 sentences max): Why can `freeVars` NOT be implemented with
-- an instance of a tagless encoding of `Expr` with a higher-order abstract
-- syntax (HOAS)?

-- in HOAS we dont know the name of the varibles. so we cant make a set of them
