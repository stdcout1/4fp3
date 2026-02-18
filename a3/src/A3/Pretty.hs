-- | Pretty-printing.
--
-- You should not need to touch any of this code to complete the assignment,
-- though feel free to peruse!
--
-- This module is meant to be imported qualified ala
--
-- > import A3.Pretty (Pretty(..))
-- > import A3.Pretty qualified as Pretty
module A3.Pretty
  (
  -- $pretty
    Pretty(..)
  , string
  , integral
  , punctuate
  , blank
  , vcat
  -- $precedence
  , Prec(..)
  , Fixity(..)
  , PrettyEnv(..)
  , parens
  ) where

import Data.Word

import Data.List (intersperse)
import Data.Monoid (Endo(..))

-- $pretty
-- * Pretty printing
class Pretty a where
  precOf :: a -> Prec

  -- | Pretty print a thing that has binding precedence.
  prettyPrecS :: PrettyEnv -> a -> ShowS

  -- | Pretty print a thing in an 'Isolated' environment.
  --
  -- This is a class method to allow implementations to
  -- offer more optimized implementations. Such implementations
  -- should satisfy the law:
  --
  -- > pretty x = prettyPrecS Isolated x ""
  pretty :: a -> String
  pretty x = prettyPrecS Isolated x ""

string :: String -> ShowS
string = showString

integral :: (Integral a) => a -> ShowS
integral a = shows (toInteger a)

-- | Insert a punctuation delimiter between elements of a list.
punctuate :: ShowS -> [ShowS] -> ShowS
punctuate delim = appEndo . foldMap Endo . intersperse delim

blank :: ShowS
blank = id

-- | Vertically concatenate.
vcat :: [ShowS] -> ShowS
vcat = punctuate (string "\n")

-- $precedence
-- * Precedence

-- | Precedences.
data Prec
  = Binder
  -- ^ Binders have the lowest precedence to let things like
  -- @forall (x : a). foo && bar@ print without parens.
  | Op Fixity Word8
  -- ^ An operator with some 'Fixity' and a precedence level.
  | Juxt
  -- ^ Juxtaposition should bind tighter than operators to let thing like
  -- @f x && g y@ print without parens.
  | Atom
  -- ^ Atoms bind tighter than anything else.
  deriving (Eq)

fixity :: Prec -> Fixity
fixity Binder = RightAssoc
fixity (Op x _) = x
fixity Juxt = LeftAssoc
fixity Atom = NonAssoc

level :: Prec -> Word
level Binder = 0
level (Op _ lvl) = 1 + fromIntegral lvl
level Juxt = 2 + fromIntegral (maxBound @Word8)
level Atom = 3 + fromIntegral (maxBound @Word8)

  -- Prec !Fixity PrecLevel
  -- -- ^ A precedence level consists of a 'Fixity' describing
  -- -- how it should associate, and a 'lvl' that describes
  -- -- how tightly it binds.

-- data PrecLevel
--   = Binder
--   -- ^ Binders have the lowest precedence to let things like
--   -- @forall (x : a). foo && bar@ print without parens.
--   | Op Word8
--   -- ^ Binary operators.
--   | Juxt
--   -- ^ Juxtaposition should bind tighter than operators to let thing like
--   -- @f x && g y@ print without parens.
--   | Atom
--   -- ^ Atoms bind tighter than anything else.

-- instance Enum PrecLevel where
--   fromEnum Binder = 0
--   fromEnum (Op lvl) = 1 + fromIntegral lvl
--   fromEnum Juxt = 2 + fromIntegral (maxBound @Word8)
--   fromEnum Atom = 3 + fromIntegral (maxBound @Word8)

--   toEnum n | n == 0 = Binder
--            | n <= 1 + fromIntegral (maxBound @Word8) = Op (fromIntegral (n - 1))
--            | n == 2 + fromIntegral (maxBound @Word8) = Juxt
--            | n == 3 + fromIntegral (maxBound @Word8) = Atom
--            | otherwise = error "toEnum: out of bounds"

-- | The fixity of a bit of synax.
data Fixity
  = NonAssoc
  | LeftAssoc
  | RightAssoc
  | Prefix
  | Postfix
  deriving (Eq)

-- | Get the left precedence of a precedence level.
precLeft :: Prec -> Word
precLeft p =
  case fixity p of
    RightAssoc -> 2*level p + 1
    Prefix -> maxBound
    _ -> 2*level p

precRight :: Prec -> Word
precRight p =
  case fixity p of
    LeftAssoc -> 2*level p + 1
    Postfix -> maxBound
    _ -> 2*level p

-- | Pretty printing environments.
data PrettyEnv
  = LeftOf Prec
  | RightOf Prec
  | Surrounded Prec
  | Isolated


-- | Do we need to insert parenstheses for something with a given precedence?
needParens :: PrettyEnv -> Prec -> Bool
needParens (LeftOf env) prec = precLeft env >= precRight prec
needParens (RightOf env) prec = precRight env >= precLeft prec
needParens (Surrounded env) prec = precRight env >= precLeft prec || precLeft env >= precRight prec
needParens Isolated _ = False

-- | Conditionally a wrap a string in parentheses if the precedence level of @a@
-- demands them.
parens :: (Pretty a) => (Prec -> a -> ShowS) -> PrettyEnv -> a -> ShowS
parens k env a =
  let prec = precOf a
  in showParen (needParens env prec) (k prec a)
