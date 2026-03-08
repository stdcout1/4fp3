class Term e where 
    lit :: Integer -> e
    add :: e -> e -> e
    sub :: e -> e -> e

newtype R = R {unR :: Integer}

instance Term R where 
    lit i = R i 
    add = abstraction (+) 
    sub lhs rhs = R ( (unR lhs) - (unR rhs) )

liftA2 :: (Integer -> Integer -> Integer) -> R -> R -> R
liftA2 f lhs rhs = R $ f (unR lhs) (unR rhs) 

-- import Data.Kind (Type)
--
-- class IExpr (repr :: Type -> Type) where
--   int :: Integer                      -> repr Integer
--   add :: repr Integer -> repr Integer -> repr Integer
--   sub :: repr Integer -> repr Integer -> repr Integer
--   mul :: repr Integer -> repr Integer -> repr Integer
--
-- newtype R a = R {unR :: a}
--
-- instance Functor R where 
--     fmap f r = R $ f $ unR r
--
-- instance Applicative R where 
--     pure a = R a
--     liftA2 f r1 r2 = R $ f (unR r1) (unR r2)
--
-- instance IExpr R where 
--     int = R
--     add = liftA2 (+)
--     sub = liftA2 (-)
--     mul = liftA2 (*)
--
-- class BExpr (repr :: Type -> Type) where
--   bool :: Bool                   -> repr Bool
--   not_ :: repr Bool              -> repr Bool
--   and_ :: repr Bool -> repr Bool -> repr Bool
--   or_  :: repr Bool -> repr Bool -> repr Bool
--
-- instance BExpr R where 
--     bool = R
--     not_ = fmap not 
--     and_ = liftA2 (&&)
--     or_ = liftA2 (||)
--
-- class Cond (repr :: Type -> Type) where 
--     if_ :: repr Bool -> repr a -> repr a -> repr a
--
-- instance Cond R where 
--     if_ cond tb eb = R $ if unR cond then unR tb else unR eb
--
-- ee :: (IExpr repr, BExpr repr, Cond repr) => repr Integer
-- ee = if_ (and_ (bool True) (bool False))
--          (int 5)
--          (add (int 3) (int 6))
--
--
