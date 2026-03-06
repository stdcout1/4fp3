module Mar3 where

import Data.Map as Map (Map, insert, lookup, toList)
import Data.Maybe (fromMaybe)
import Numeric.Natural

-- LO: Partial evaluation on tiny purpose-made languages
-- before scaling up
--
-- we simplfiy things that we know at compile time for our langauges.
-- this is a simple example
--
-- -- just addition
-- -- Language to model addition of open terms (i.e. with variables)
-- data ALang = I Integer | A ALang ALang | V String
--     deriving Show
-- data AN = AN Integer [(Natural, String)]
--     deriving Show
--   -- Hmm, Map String Natural ?
--
-- -- idea: a "program" in ALang is always equivalent to one in AN
-- pevalA :: ALang -> ALang
-- pevalA = reifyA . normalA
-- -- we convert to AN then back to ALang. that transformation can simplify
--
-- normalA :: ALang -> AN
-- normalA t = accA t (AN 0 [])
--
-- accA :: ALang -> AN -> AN
-- accA (I n)      (AN i lst) = AN (i + n) lst
-- accA (A l r)    (AN i lst) = accA r $ accA l (AN i lst)
-- accA (V v)      (AN i lst) = AN i ((1, v) : lst)
--
-- reifyA :: AN -> ALang
-- reifyA (AN i l) = rest
--     where
--         rest :: ALang
--         rest =  foldl A (I i) $ map toAdd l
--         -- rest =  foldr A (I i) $ map toAdd l
--         -- foldl puts 5 on left
--         toAdd :: (Natural, String) -> ALang
--         toAdd (0, _) = I 0
--         toAdd (1, v) = V v
--         toAdd (n, v) = A (V v) (toAdd (n-1, v))
--
-- exA :: ALang
-- exA = A (A (A y (I (-3))) (A (A x (I 5)) (I 3))) x
--     where x = V "x"
--           y = V "y"
--
-- exAN :: AN
-- exAN = AN 5 ((2, "x") : (1, "y") : [])

-- we can do this with map as well...

data ALang = I Integer | A ALang ALang | V String
  deriving (Show)

data AN = AN Integer (Map.Map String Natural)
  deriving (Show)

-- Hmm, Map String Natural ?

-- idea: a "program" in ALang is always equivalent to one in AN
pevalA :: ALang -> ALang
pevalA = reifyA . normalA

-- we convert to AN then back to ALang. that transformation can simplify

normalA :: ALang -> AN
normalA t = accA t (AN 0 mempty)

accA :: ALang -> AN -> AN
accA (I n) (AN i m) = AN (i + n) m
accA (A l r) (AN i m) = accA r $ accA l (AN i m)
-- change thsi to a look up
-- below has lots of duplication so we edit it
-- accA (V v)      (AN i m) =
--     case (Map.lookup v m) of
--         Just n -> AN i $ Map.insert v (n + 1) m
--         Nothing -> AN i $ Map.insert v 1 m
accA (V v) (AN i m) = AN i $ Map.insert v (mult + 1) m
  where
    mult = fromMaybe 0 $ Map.lookup v m

reifyA :: AN -> ALang
reifyA (AN i m) = rest
  where
    rest :: ALang
    rest = foldl A (I i) $ map toAdd $ Map.toList m
    -- rest =  foldr A (I i) $ map toAdd l
    -- foldl puts 5 on left
    toAdd :: (String, Natural) -> ALang
    toAdd (_, 0) = I 0
    toAdd (v, 1) = V v
    toAdd (v, n) = A (V v) (toAdd (v, n - 1))

exA :: ALang
exA = A (A (A y (I (-3))) (A (A x (I 5)) (I 3))) x
  where
    x = V "x"
    y = V "y"

-- exAN :: AN
-- exAN = AN 5 ((2, "x") : (1, "y") : [])

-- just multiplication
-- Language to model multiplication of open terms (i.e. with variables)
-- data MLang = I Integer | M Integer Integer | V String
-- data MN = MN Integer [(Natural, String)]
-- Hmm, Map String Natural ?

-- pevalM :: MLang -> MLang
-- pevalM = undefined
--
-- normalM :: MLang -> MN
-- normalM = undefined
--
-- accM :: MLang -> MN -> MN
-- accM = undefined
--
-- reifyM :: MN -> MLang
-- reifyM (MN i l) = undefined
--
-- what about and?

-- what about add and mul?
-- what about sub? divide? xor?
--
-- associative, commutative, idempotent, nilpotent, unital, zero
