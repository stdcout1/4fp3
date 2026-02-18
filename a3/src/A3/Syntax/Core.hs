-- | Terms.
module A3.Syntax.Core
  ( Name
  , getName
  , freshen
  , Binder(..)
  , Type(..)
  , Term(..)
  -- $renaming
  , Rename(..)
  , DontRename(..)
  ) where

import A3.Permutation (Permutation)
import A3.Permutation qualified as P
import A3.Pretty (Pretty(..))
import A3.Pretty qualified as Pretty

import Data.Set (Set, size)
import Data.Set qualified as Set

-- | A name.
newtype Name =
  UnsafeName { getName :: String }
  -- ^ Make a name from a string.
  --
  -- This constructor should be treated as an
  -- unsafe function, as it can be used to very
  -- easily violate name freshness conditions.
  --
  -- Users are recommended to use @freshen@ instead.
  deriving (Show, Eq, Ord)

-- | Turn an arbitrary string into a name that is fresh
-- with respect to a set of names.
--
-- Laws:
-- > Set.notMember (freshen x nms) nms
freshen :: String -> Set Name -> Name
freshen x avoid = UnsafeName {getName= x ++ show(size avoid)}

-- $binders
-- * Binders

-- | An binding form.
data Binder ann a = Binder Name ann a
  deriving (Show)


-- | Equality of 'Binder's is given by alpha equivalence.
instance (Rename a, Eq ann, Eq a) => Eq (Binder ann a) where
  (Binder x1 ann1 a1) == (Binder x2 ann2 a2)
    | x1 == x2 = a1 == a2
    | otherwise = ann1 == ann2 && a1 == rename a2 (P.swap x1 x2)

-- | Types.
data Type
  = Prop
  | Fn Type Type
  deriving (Show, Eq)

-- | Terms.
data Term
  = Var Name
  -- ^ Variables.
  | Lam (Binder Type Term)
  -- ^ Lambda abstraction.
  | App Term Term
  -- ^ Application.
  | Top
  -- ^ True.
  | Bot
  -- ^ False.
  | And Term Term
  -- ^ Conjunction.
  | Or Term Term
  -- ^ Disjunction.
  | Implies Term Term
  -- ^ Implication.
  | ForAll (Binder Type Term)
  -- ^ Universal quantification.
  | Exists (Binder Type Term)
  -- ^ Existential quantification.
  | Eq Term Term Type
  -- ^ Equality.
  deriving (Show, Eq)

-- $renaming
-- * Renaming

class Rename a where
  -- | Permute all the names in @a@.
  --
  -- Laws:
  -- > rename a (p1 <> p2) == rename (a p1) p2
  -- > rename a mempty == a
  rename :: a -> Permutation Name -> a

  -- | Get the free variables of @a@, EG: a set of names @xs@
  -- such that for all permutations @p@ with
  --
  -- > Set.disjoint (P.freeVars p) (freeVars a)
  --
  -- we have @rename a p == a@.
  freeVars :: a -> Set Name

instance Rename () where
    rename _ _ = ()
    freeVars _ = mempty 

instance (Rename a, Rename b) => Rename (a, b) where
    rename (a, b) p = (rename a p, rename b p)
    freeVars (a, b) = Set.union (freeVars a) ( freeVars b)

instance (Rename a, Rename b, Rename c) => Rename (a, b, c) where
    rename (a, b, c) p = (rename a p, rename b p, rename c p)
    freeVars (a, b, c) = Set.union (Set.union (freeVars a) ( freeVars b)) (freeVars c)

instance (Rename a, Rename b) => Rename (Either a b) where
    rename e p = fmap (`rename` p) e
    freeVars (Left a) = freeVars a
    freeVars (Right b) = freeVars b

instance (Rename a) => Rename [a] where
    rename xs p = fmap (`rename` p) xs
    freeVars xs = Set.unions (fmap freeVars xs)

instance Rename Name where
    -- to rename we just find the associated permunation
    rename = P.permute 
    -- obviously the free vaible is just itself
    freeVars = Set.singleton

instance (Rename a) => Rename (Binder ann a) where
    -- we rename the binding varible 
    rename (Binder n ann a) p = Binder n ann (rename a p) 
    freeVars (Binder n _ _) = Set.singleton n 

instance Rename Term where
    -- recursivly rename
    rename (Var n) p = Var (rename n p) 
    rename (Lam binder ) p = Lam (rename binder p)
    rename (App t1 t2) p = App (rename t1 p) (rename t2 p)
    rename Top _ = Top
    rename Bot _ = Bot
    rename (And t1 t2) p = And (rename t1 p) (rename t2 p)
    rename (Or t1 t2) p = Or (rename t1 p) (rename t2 p)
    rename (Implies t1 t2) p = Implies (rename t1 p) (rename t2 p)
    rename (ForAll binder) p = ForAll (rename binder p)
    rename (Exists binder) p = Exists (rename binder p)
    rename (Eq t1 t2 ty) p = Eq (rename t1 p) (rename t2 p) ty 

    -- recursivly find the freeVars and merge themp up
    freeVars (Var n) = freeVars n 
    freeVars (Lam binder ) = freeVars binder
    freeVars (App t1 t2) = Set.union (freeVars t1 ) (freeVars t2 )
    freeVars Top = mempty
    freeVars Bot = mempty
    freeVars (And t1 t2) = Set.union (freeVars t1 ) (freeVars t2 )
    freeVars (Or t1 t2) = Set.union (freeVars t1 ) (freeVars t2 )
    freeVars (Implies t1 t2) = Set.union (freeVars t1 ) (freeVars t2 )
    freeVars (ForAll binder) = freeVars binder 
    freeVars (Exists binder) = freeVars binder
    freeVars (Eq t1 t2 _) = Set.union (freeVars t1 ) (freeVars t2 ) 


-- | Ignore something for the purposes of renaming.
--
-- This is used internally in some hedgehog generators,
-- and should not be relied on in user code.
newtype DontRename a = DontRename a
  deriving (Show, Eq, Ord)

instance Rename (DontRename a) where
  rename x _ = x
  freeVars _ = Set.empty

-- * Pretty printing
--

instance Pretty Name where
  precOf _ = Pretty.Atom

  prettyPrecS _ x = showString (getName x)

instance Pretty Type where
  precOf Prop = Pretty.Atom
  precOf (Fn {}) = Pretty.Op Pretty.RightAssoc 0

  prettyPrecS = Pretty.parens \prec tp ->
    case tp of
      Prop -> Pretty.string "Prop"
      (Fn dom cod) ->
        prettyPrecS (Pretty.LeftOf prec) dom
        . Pretty.string " -> "
        . prettyPrecS (Pretty.RightOf prec) cod

instance Pretty Term where
  precOf (Var {}) = Pretty.Atom
  precOf (Lam {}) = Pretty.Binder
  precOf (App {}) = Pretty.Juxt
  precOf Top = Pretty.Atom
  precOf Bot = Pretty.Atom
  precOf (And {}) = Pretty.Op Pretty.LeftAssoc 3
  precOf (Or {}) = Pretty.Op Pretty.LeftAssoc 2
  precOf (Implies {}) = Pretty.Op Pretty.RightAssoc 0
  precOf (Exists {}) = Pretty.Binder
  precOf (ForAll {}) = Pretty.Binder
  precOf (Eq {}) = Pretty.Op Pretty.NonAssoc 4

  prettyPrecS = Pretty.parens \prec tm ->
    case tm of
      Var x ->
        prettyPrecS Pretty.Isolated x
      Lam body ->
        Pretty.string "\\" . prettyAnnBinder prec body
      App tm1 tm2 ->
        prettyBinary prec tm1 (Pretty.string " ") tm2
      Top ->
        Pretty.string "true"
      Bot ->
        Pretty.string "false"
      And tm1 tm2 ->
        prettyBinary prec tm1 (Pretty.string " && ") tm2
      Or tm1 tm2 ->
        prettyBinary prec tm1 (Pretty.string " || ") tm2
      Implies tm1 tm2 ->
        prettyBinary prec tm1 (Pretty.string " => ") tm2
      Exists body ->
        Pretty.string "exists " . prettyAnnBinder prec body
      ForAll body ->
        Pretty.string "forall " . prettyAnnBinder prec body
      Eq tm1 tm2 tp ->
        prettyBinary prec tm1 (Pretty.string " ={" . prettyPrecS Pretty.Isolated tp . Pretty.string "} ") tm2
    where
      prettyAnnBinder :: Pretty.Prec -> Binder Type Term -> ShowS
      prettyAnnBinder prec (Binder x tp tm) =
        Pretty.string "("
        . Pretty.prettyPrecS Pretty.Isolated x
        . Pretty.string ":"
        . Pretty.prettyPrecS Pretty.Isolated tp
        . Pretty.string "). "
        . prettyPrecS (Pretty.RightOf prec) tm

      prettyBinary :: Pretty.Prec -> Term -> ShowS -> Term -> ShowS
      prettyBinary prec ltm op rtm =
        Pretty.prettyPrecS (Pretty.LeftOf prec) ltm
        . op
        . Pretty.prettyPrecS (Pretty.RightOf prec) rtm
