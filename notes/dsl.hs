import Data.Kind (Type)

class IExpr (repr :: Type -> Type) where
  int :: Integer                      -> repr Integer
  add :: repr Integer -> repr Integer -> repr Integer
  sub :: repr Integer -> repr Integer -> repr Integer
  mul :: repr Integer -> repr Integer -> repr Integer

newtype R a = R {unR :: a}

instance Functor R where 
    fmap f r = R $ f $ unR r

instance Applicative R where 
    pure a = R a
    liftA2 f r1 r2 = R $ f (unR r1) (unR r2)

instance IExpr R where 
    int = R
    add = liftA2 (+)
    sub = liftA2 (-)
    mul = liftA2 (*)

class BExpr (repr :: Type -> Type) where
  bool :: Bool                   -> repr Bool
  not_ :: repr Bool              -> repr Bool
  and_ :: repr Bool -> repr Bool -> repr Bool
  or_  :: repr Bool -> repr Bool -> repr Bool

instance BExpr R where 
    bool = R
    not_ = fmap not 
    and_ = liftA2 (&&)
    or_ = liftA2 (||)

class Cond (repr :: Type -> Type) where 
    if_ :: repr Bool -> repr a -> repr a -> repr a

instance Cond R where 
    if_ cond tb eb = R $ if unR cond then unR tb else unR eb

ee :: (IExpr repr, BExpr repr, Cond repr) => repr Integer
ee = if_ (and_ (bool True) (bool False))
         (int 5)
         (add (int 3) (int 6))

class FExpr (repr :: Type -> Type) where 
    -- f
    lam :: (repr a -> repr b) -> repr (a -> b)
    -- f (x: a) -> y : b
    app :: repr (a -> b) -> (repr a -> repr b)

instance FExpr R where 
    lam f = R $ unR . f . R
    app = liftA2 ($)

class Fix repr where
   fix_ :: (repr (a -> b) -> repr (a -> b)) -> repr (a -> b)

fact :: (Fun repr, IExpr repr, BExpr repr, Fix repr,
  IOrder repr, Cond repr) => repr (Integer -> Integer)
fact = fix_ (\self -> lam (\i ->
   if_ (eq i (int 0))
       (int 1)
       (mul i (app self (sub i (int 1))))))

instance Fix R where
  fix_ f = fix f


