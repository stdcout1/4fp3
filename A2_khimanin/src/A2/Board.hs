{-# LANGUAGE OverloadedRecordDot #-}

module A2.Board
  ( Board (..),
    Grid,
    freshBoard,
    refreshBoard,
    isGameOver,
    emptySpots,
    SlideDirection (..),
    twoDistinctRandomElem,
    slide,
    pushRowRight,
    slideRowLeft,
    slideRowRight,
    mergeRowRight,
    slideGrid

  )
where

import A2.Grid4x4 (Grid4x4, at, indices, transpose, update)
import A2.Quadruple (Index, Quadruple (..), getQuad, indices, reverseQuad)
import Numeric.Natural (Natural)
import System.Random (StdGen, random, randomR)
import Prelude hiding (Left, Right)

-- | A single row of the board is a 'Quadruple' of 'Word's.
type Row = Quadruple Word

-- | The 2048 grid is a \(4\times{}4\) matrix of unsigned integers.
type Grid = Grid4x4 Word

-- | The opaque game state containing the current grid and a random number
-- generator. The RNG allows for deterministic replays.
data Board = Board
  { grid :: Grid,
    rng :: StdGen
  }
  deriving (Eq, Show)

-- | The 4 possible directions a player can slide tiles.
data SlideDirection = SUp | SDown | SLeft | SRight
  deriving (Eq, Show)

-- gets two random elemets using a generator
twoDistinctRandomElem :: StdGen -> [a] -> (a, a, StdGen)
twoDistinctRandomElem gen xs =
  let n = length xs
      -- select two numbers int the range of the list
      (i, g1) = randomR (0, n - 1) gen
      (j', g2) = randomR (0, n - 2) g1
      -- ensure we can still get j to be n - 2.
      -- this is to prevent us from making j impossible to select length xs
      -- (ie changing the ods )
      j = if j' >= i then j' + 1 else j'
   in (xs !! i, xs !! j, g2)

-- | Initialize a fresh 2048 game board, a 'Grid' of 0s with two tiles (2s or
-- 4s) placed randomly on the board.
freshBoard :: StdGen -> Board
freshBoard gen =
  -- we select two random numbers (and making sure to update the stdgen).
  let (randomNumber1, gen1) = random gen :: (Float, StdGen)
      (randomNumber2, gen2) = random gen1 :: (Float, StdGen)
      (randomPos1, randomPos2, gen3) = twoDistinctRandomElem gen2 A2.Grid4x4.indices
      zeros = pure (pure 0) :: Grid4x4 Word
   in -- setup zeros then put the random two tiles in
      Board {grid = update randomPos1 (if randomNumber1 <= 0.9 then 2 else 4) (update randomPos2 (if randomNumber2 <= 0.9 then 2 else 4) zeros), rng = gen3}

-- | Resets the game board, preserving the RNG.
refreshBoard :: Board -> Board
refreshBoard Board {grid = _, rng = gen} =
  let (randomNumber1, gen1) = random gen :: (Float, StdGen)
      (randomNumber2, gen2) = random gen1 :: (Float, StdGen)
      (randomPos1, randomPos2, gen3) = twoDistinctRandomElem gen2 A2.Grid4x4.indices
      zeros = pure (pure 0) :: Grid4x4 Word
   in -- setup zeros then put the random two tiles in
      Board {grid = update randomPos1 (if randomNumber1 <= 0.9 then 2 else 4) (update randomPos2 (if randomNumber2 <= 0.9 then 2 else 4) zeros), rng = gen3}

-- | Finds all 'empty' positions on a board (i.e., spots where the tile value is
-- 0).
emptySpots :: Board -> [(Index, Index)]
emptySpots Board {grid = grid, rng = _} = filter (\i -> at grid i == 0) A2.Grid4x4.indices

-- | Checks if no further slides on a 'Board' change the board. Alternatively
-- understood, checks if the board is full and no adjacent tiles can be merged.
isGameOver :: Board -> Bool
isGameOver board =
  not
    ( any
        ( \d -> case slide board d of
            (_, True) -> True
            _ -> False
        )
        [SDown, SLeft, SRight, SUp]
    )

pushRowRight :: Row -> Row
pushRowRight q =
  foldr
    -- fold on the non zeros mocing our v into place.
    ( \v acc ->
        case acc of
          Quad a b c d
            | d == 0 -> Quad a b c v
            | c == 0 -> Quad a b v d
            | b == 0 -> Quad a v c d
            | otherwise -> Quad v b c d
    )
    (pure 0 :: Quadruple Word)
    -- get all non zeros
    (filter (/= 0) [getQuad i q | i <- A2.Quadruple.indices])

-- ensure the neighbour is not zero and is equal, then merge them
mergeRowRight :: Row -> Row
mergeRowRight (Quad a b c d)
  -- merge right pair, then (optionally) merge left pair too
  | c /= 0 && c == d =
      let (a', b') =
            if a /= 0 && a == b
              then (0, b + b)
              else (a, b)
       in Quad a' b' 0 (d + d)

  -- merge middle pair (overlaps, so this is the only merge)
  | b /= 0 && b == c =
      Quad a 0 (c + c) d

  -- merge left pair
  | a /= 0 && a == b =
      Quad 0 (b + b) c d

  | otherwise =
      Quad a b c d

-- a slide action is basicaly
-- compress (ie remove zeros in the direction)
-- perform any possible merges
-- compress again to remove any zeros made by the merges.
slideRowRight :: Row -> Row
slideRowRight r = pushRowRight $ mergeRowRight $ pushRowRight r
--                  compress     merge near by   compress

-- a mirror of above...

slideRowLeft :: Row -> Row
slideRowLeft r = reverseQuad $ slideRowRight $ reverseQuad r

slideGrid :: SlideDirection -> Grid -> Grid
-- just apply the slide of each row
slideGrid SLeft = fmap slideRowLeft
slideGrid SRight = fmap slideRowRight
-- transpose it, and do the same thing.
slideGrid SUp = transpose . fmap slideRowLeft . transpose
slideGrid SDown = transpose . fmap slideRowRight . transpose

-- | Performs a row slide action in a specific direction, returning:
--
-- 1. The resulting 'Board' of the slide with a randomly spawned tile.
-- 2. A 'Bool' indicating if the slide affected the board in any way.
slide :: Board -> SlideDirection -> (Board, Bool)
slide b dir =
  -- make the new grid, then spawn the tile, and return the board with the update grid.
  let newGrid = slideGrid dir b.grid
  -- only spawn a new tile if the board changes...
   in if newGrid == b.grid
        then (b, False)
        else (spawnTile (Board {grid = newGrid, rng = b.rng}), True)

-- | Randomly spawns a tile on a board, with a 90% chance of being a 2 and a 10%
-- chance of being a 4. Only spawns a tile if there is at least 1 available spot
-- on the board.
spawnTile :: Board -> Board
spawnTile Board {grid = g, rng = gen} =
  let (randomNumber1, gen1) = random gen :: (Float, StdGen)
      (randomPos1, _, gen2) = twoDistinctRandomElem gen1 (emptySpots Board {grid = g, rng = gen})
   in -- setup zeros then put the random two tiles in
      Board {grid = update randomPos1 (if randomNumber1 <= 0.9 then 2 else 4) g, rng = gen2}

ntimes :: Natural -> (a -> a) -> (a -> a)
ntimes 0 _ = id
ntimes n f = f . ntimes (n - 1) f
