module A2.Quadruple
  ( Quadruple (..),
    Index (..),
    getQuad,
    setQuad,
    reverseQuad,
    indices,
  )
where

-- | A fixed-size vector of exactly four elements. Used to represent rows,
-- columns, and the 4x4 grid itself.
data Quadruple a = Quad a a a a
  deriving (Eq, Show)

-- | An index type for addressing elements in a 'Quadruple'. Eliminates runtime
-- bounds-checking errors.
data Index = I0 | I1 | I2 | I3
  deriving (Eq, Show)

instance Functor Quadruple where
  -- | Applies a function to every element in the Quadruple.
  fmap f (Quad a b c d) = Quad (f a) (f b) (f c) (f d)

instance Applicative Quadruple where
  -- | Fills in a 'Quadruple' with a single element.
  pure e = Quad e e e e
  -- | Applies functions to values at matching indices.
  (Quad f1 f2 f3 f4) <*> (Quad x1 x2 x3 x4) =
    Quad (f1 x1) (f2 x2) (f3 x3) (f4 x4)

instance Foldable Quadruple where
  -- | Folds a 'Quadruple' from left to right.
  foldr f acc (Quad a b c d) = f a (f b (f c (f d acc))) 
  foldl f acc (Quad a b c d) = f (f (f (f acc a) b) c) d

-- | Finds the element of a 'Quadruple a' at a specific 'Index'.
getQuad :: Index -> Quadruple a -> a
getQuad I0 (Quad a _ _ _) = a 
getQuad I1 (Quad _ b _ _) = b 
getQuad I2 (Quad _ _ c _) = c 
getQuad I3 (Quad _ _ _ d) = d 

-- | Creates a duplicate 'Quadruple', replacing the value at a specific 'Index'
-- with another
setQuad :: Index -> a -> Quadruple a -> Quadruple a
setQuad I0 v (Quad _ b c d) = Quad v b c d 
setQuad I1 v (Quad a _ c d) = Quad a v c d 
setQuad I2 v (Quad a b _ d) = Quad a b v d 
setQuad I3 v (Quad a b c _) = Quad a b c v 

-- | Reverses a 'Quadruple'.
reverseQuad :: Quadruple a -> Quadruple a
reverseQuad (Quad a b c d) = Quad d c b a

-- | An exhaustive list of indices, i.e., [I0 .. I3].
indices :: [Index]
indices = [I0, I1, I2, I3]
