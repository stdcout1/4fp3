-- | Substitutions.
--
-- This module is indended to be imported qualified as
-- > import A3.Syntax.Substitution (Substitution, Substitute(..))
-- > import A3.Syntax.Substitution qualified as Sub
module A3.Syntax.Substitution
  ( Substitution(..)
  , vars
  , terms
  , single
  , restrict
  , Substitute(..)
  ) where

import A3.Syntax.Core
import A3.Permutation qualified as P

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set
import A3.Permutation (swap)

newtype Substitution = Substitution { unSubstitution :: Map Name Term }
  deriving (Show, Eq)

-- | Semigroup instance is substitution composition.
instance Semigroup Substitution where
  (Substitution sub1) <> sub2 = Substitution $
    -- Map.union is left-biased, so we only add entries from
    -- @sub2@ if they were missing from @sub1@.
    Map.union (Map.map (`substs` sub2) sub1) (unSubstitution sub2)

-- | @mempty@ is the empty substitution.
instance Monoid Substitution where
  mempty = Substitution Map.empty

-- | Renaming acts on a substitution by permuting both the names to be
-- substituted and the terms.
instance Rename Substitution where
  rename (Substitution sub) p = Substitution $ Map.fromList $ (`rename` p) <$> Map.toList sub
  freeVars (Substitution sub) = Map.foldMapWithKey (\x a -> Set.union (freeVars x) (freeVars a)) sub

-- | The set of all variables that a substitution @sub@ acts on.
vars :: Substitution -> Set Name
vars (Substitution sub) = Map.keysSet sub

-- | All terms in a substitution.
terms :: Substitution -> [Term]
terms (Substitution sub) = Map.elems sub

-- $create
-- * Creating Substitutions

-- | Create a single substitution.
single :: Name -> Term -> Substitution
single x t = Substitution (Map.singleton x t)

-- | Restrict a substitution to only act on a set of names.
restrict :: Set Name -> Substitution -> Substitution
restrict nms (Substitution sub) = Substitution (Map.restrictKeys sub nms)

-- $substitute
-- * Actions of Substitutions

class (Rename a) => Substitute a where
  -- | Substitute in @a@.
  --
  -- Laws:
  -- substs a mempty == a
  -- substs a (sub1 <> sub2) == substs (substs a sub1) sub2
  -- rename (substs a sub) p == substs (rename a p) (rename sub p)
  -- freeVars (substs a sub) == Set.union (freeVars (terms $ restrict (freeVars a) sub)) (Set.difference (freeVars a) (vars sub))
  substs :: a -> Substitution -> a

  -- | Perform a single substitution.
  --
  -- This is provided as a class method to
  -- allow for more efficient implementations.
  --
  -- Laws:
  -- > subst a nm tm == substs a (single nm tm)
  subst :: a -> Name -> Term -> a
  subst a nm tm = substs a (single nm tm)

instance (Substitute a, Substitute b) => Substitute (a, b) where
    -- just map over both
    -- substs t s = fmap (`substs` s) t
    substs (a, b) s = (substs a s, substs b s)

instance (Substitute a) => Substitute (Binder ann a) where
    -- use freshen to pick a new binder name (name). 
    -- it should not be in freeVar (s) and freeVar(ann).
    -- swap all old binder names with new one 
    -- return the new binder while substitiing into a. 
    substs (Binder n ann a) s = 
        let 
            newName = freshen (getName n) (Set.union (freeVars s) (freeVars a))
            newA = rename a (swap n newName)
        in Binder newName ann (substs newA s) 

instance Substitute Term where
    -- if there is a mapping to a term then replace it else dont change it. 
    substs (Var n) (Substitution s) = Map.findWithDefault (Var n) n s
    -- substitie under the lambda, keep in the mind the binder above...
    substs (Lam binder) s = Lam (substs binder s) 
    substs (App t1 t2) s = App (substs t1 s) (substs t2 s)
    substs Top s = Top 
    substs Bot s = Bot  
    substs (And t1 t2) s = And (substs t1 s) (substs t2 s)
    substs (Or t1 t2) s = Or (substs t1 s) (substs t2 s)
    substs (Implies t1 t2) s = Implies (substs t1 s) (substs t2 s)
    substs (ForAll binder) s = ForAll (substs binder s) 
    substs (Exists binder) s = Exists (substs binder s) 
    substs (Eq t1 t2 ty) s = Eq (substs t1 s) (substs t2 s) ty
