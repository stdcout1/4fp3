module A6.Fix
  ( Fix (..),
  )
where

-- | The fixed point of a functor.
newtype Fix f = In {out :: f (Fix f)}
