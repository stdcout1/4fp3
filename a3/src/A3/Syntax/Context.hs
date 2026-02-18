-- | Contexts.
--
-- You should not need to modify any of this code to complete the assignment,
-- though you will need to call it.
module A3.Syntax.Context
  ( Context
  , empty
  , extend
  , lookup
  , toList
  ) where

import Prelude hiding (lookup)

import A3.Syntax.Core

import Data.Map (Map)
import Data.Map.Strict qualified as Map

-- | Contexts.
newtype Context a = Context (Map Name a)
  deriving (Show)

-- | The empty context.
empty :: Context a
empty = Context Map.empty

-- | Extend a context with a new binding.
--
-- Note that the provided string is only a name hint:
-- the actual name inserted into the context is returned
-- along with the extended context.
extend :: Context a -> String -> a -> (Name, Context a)
extend (Context ctx) str a =
  let nm = freshen str (Map.keysSet ctx)
  in (nm, Context $ Map.insert nm a ctx)

-- | Look up a name in a context.
lookup :: Name -> Context a -> Maybe a
lookup nm (Context ctx) = Map.lookup nm ctx

-- | Get a list of bindings in a context.
toList :: Context a -> [(Name, a)]
toList (Context ctx) = Map.toList ctx
