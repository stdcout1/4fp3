module A2.Grid4x4
  ( Grid4x4,
    at,
    update,
    indices,
    transpose,
    followIndex 
  )
where

import A2.Quadruple (Index (..), Quadruple (..), getQuad, setQuad)
import qualified A2.Quadruple as Q (indices)

-- | A /row-major/ 4x4 grid.
type Grid4x4 a = Quadruple (Quadruple a)

-- | Accessor for the element at the \((x,y)\) coordinate.
at :: Grid4x4 a -> (Index, Index) -> a
at g (x, y) = getQuad x (getQuad y g)    

-- | Returns a new 'Grid4x4' with the value at \((x,y)\) replaced.
update :: (Index, Index) -> a -> Grid4x4 a -> Grid4x4 a
update (x, y) val g = setQuad y (setQuad x val current) g 
    where current = getQuad y g

-- | Enumerates all coordinates (in row-major order).
indices :: [(Index, Index)]
indices = liftA2 (,) Q.indices Q.indices

-- | Makes a quad based on a position mapping.
followIndex :: (Index -> a) -> Quadruple a
followIndex f = Quad (f I0) (f I1) (f I2) (f I3)

-- | Matrix transpose operation. Swaps values such that \(M_{ij}\) becomes
-- \(M_{ji}\).
transpose :: Grid4x4 a -> Grid4x4 a
transpose q = followIndex (\x-> followIndex (\y -> at q (x, y)) )
-- we map our old x, y to new y, x. 
