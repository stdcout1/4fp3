{-# LANGUAGE OverloadedRecordDot #-}

module A2.Bot
  ( MyBot,
    botInfoMsg,
    initBot,
    nextMove,
    emptyCellsInRow,
    mergesInRow,
    sumInRow,
    emptyCellsInCol,
    mergesInCol,
    sumInCol, 
    monoDownCol,
    monoUpCol,
    monoLeftRow,
    monoRightRow

  )
where

import A2.Board (Board (..), Grid, SlideDirection (..), emptySpots, grid, slide, slideGrid, pushRowRight)
import A2.Grid4x4 (at, indices, transpose, update)
import A2.Quadruple (Index (..), Quadruple (..), getQuad)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)

-- --------------------------------------------
-- Bot state
-- --------------------------------------------

newtype MyBot = MyBot {lastMove :: SlideDirection}
  deriving (Eq, Show)

botInfoMsg :: MyBot -> String
botInfoMsg _ = "Expectimax"

initBot :: Board -> MyBot
initBot _ = MyBot SLeft

-- implementation of the Expectimax algorithm desribed in https://www.baeldung.com/cs/2048-algorithm

-- the website uses three, my slow laptop can only handle 2 :(
depthLimitDefault :: Int
depthLimitDefault = 2

--some scores which are explained below...
fixedScore, emptyScore, mergesScore, monotonicityScore, sumScore :: Float
fixedScore = 10
emptyScore = 60
mergesScore = 35
monotonicityScore = 2
sumScore = 0.05

nextMove :: Board -> MyBot -> (MyBot, SlideDirection)
nextMove b _bot =
  let dirs = [SUp, SDown, SLeft, SRight]
      -- find the max by comparing the the possible score in each direction
      (bestDir, _) = maximumBy (comparing snd) $ map (\dir -> (dir, calculateMoveScore b 0 depthLimitDefault dir)) dirs
   in (MyBot bestDir, bestDir)

-- CalculateMoveScore(board, currentDepth, depthLimit)
calculateMoveScore :: Board -> Int -> Int -> SlideDirection -> Float
calculateMoveScore b currentDepth limit dir =
  let (b', changed) = slide b dir
   in if not changed
        -- this is to prevent to bot from spamming a default move. it will instead 
        -- will try a diffrent move a actually end the game instead of freezeing
        then -1/0
        else generateScore b' (currentDepth + 1) limit

-- GenerateScore(board, currentDepth, depthLimit)
generateScore :: Board -> Int -> Int -> Float
generateScore b currentDepth limit
  | currentDepth >= limit = calculateFinalScore b
  | otherwise =
      foldl
        -- sum up the possible score accountiing for the probability of each tile
        ( \acc pos ->
            acc
              + 0.9
                * calculateBestMoveScore
                  (Board {grid = update pos 2 b.grid, rng = b.rng})
                  currentDepth
                  limit
              + 0.1
                * calculateBestMoveScore
                  (Board {grid = update pos 4 b.grid, rng = b.rng})
                  currentDepth
                  limit
        )
        0
        (emptySpots b)

-- helps to find find the max over moves according to the website
calculateBestMoveScore :: Board -> Int -> Int -> Float
calculateBestMoveScore b currentDepth limit =
  foldl
    (\best dir -> max best (calculateMoveScore b currentDepth limit dir))
    0
    [SUp, SDown, SLeft, SRight]

--base case: 
-- we define values that assign a score to each type of move 
-- if we tune these values we can perhaps make the algorithm better 
calculateFinalScore :: Board -> Float
calculateFinalScore b =
  let rowScore =
        foldl
          ( \acc r ->
              acc
                -- constant baseline per row/col (mainly breaks ties / stabilizes score)
                + fixedScore
                -- empty cells means the game is further from being over so we should 
                -- reward for it
                + emptyScore * fromIntegral (emptyCellsInRow b r)
                -- how many merges can be done from this state. more merges is usally better 
                + mergesScore * fromIntegral (mergesInRow b r)
                -- penalize disorder: encourages monotone rows/cols so large tiles
                -- stay clustered (usually in a corner). We take min(left,right)
                -- because either monotone direction is acceptable.
                - monotonicityScore
                  * fromIntegral
                    (min (monoLeftRow b r) (monoRightRow b r))
                -- small penalty on total mass in the row: discourages “spread out”
                -- boards with lots of medium tiles and helps prefer consolidation.
                -- keep this weight small so we don't fight necessary growth.
                - sumScore * fromIntegral (sumInRow b r)
          )
          0
          [I0, I1, I2, I3]

      colScore =
        foldl
          ( \acc c ->
              acc
                + fixedScore
                + emptyScore * fromIntegral (emptyCellsInCol b c)
                + mergesScore * fromIntegral (mergesInCol b c)
                - monotonicityScore
                  * fromIntegral
                    (min (monoUpCol b c) (monoDownCol b c))
                - sumScore * fromIntegral (sumInCol b c)
          )
          0
          [I0, I1, I2, I3]
   in rowScore + colScore

--some very basic helpers

emptyCellsInRow :: Board -> Index -> Int
emptyCellsInRow Board {rng = _, grid = g} row =
  length $
    filter
      (\(x, y) -> y == row && at g (x, y) == 0)
      A2.Grid4x4.indices

emptyCellsInCol :: Board -> Index -> Int
emptyCellsInCol Board {rng = r, grid = g} =
  emptyCellsInRow (Board {rng = r, grid = transpose g})

-- number of merges that would occur in one slide of this row (right logic)
-- we just copy our merge logic but return a number instead
mergesInRow :: Board -> Index -> Int
mergesInRow b row =
  mergesAfterPushRight (pushRowRight (getQuad row b.grid))
  where
    mergesAfterPushRight (Quad a b c d)
      -- right pair merges; left pair may also merge (disjoint)
      | c /= 0 && c == d =
          if a /= 0 && a == b then 2 else 1
      -- middle pair merges (overlaps, so only one)
      | b /= 0 && b == c = 1
      -- left pair merges
      | a /= 0 && a == b = 1
      | otherwise        = 0

mergesInCol :: Board -> Index -> Int
mergesInCol Board {rng = r, grid = g} =
  mergesInRow (Board {rng = r, grid = transpose g})

sumInRow :: Board -> Index -> Word
sumInRow b row =
  sum $
    map
      (\(x, y) -> at b.grid (x, y))
      (filter (\(_, y) -> y == row) A2.Grid4x4.indices)

sumInCol :: Board -> Index -> Word
sumInCol Board {rng = r, grid = g} =
  sumInRow (Board {rng = r, grid = transpose g})

-- monotonicity:
-- We compute a "violation penalty".
-- For left-monotone (non-increasing left->right): penalty adds when next > curr.
-- For right-monotone (non-decreasing left->right): penalty adds when curr > next.
-- this is to help compute a proression styke property.
monoPenaltyDec :: Word -> Word -> Int
monoPenaltyDec a b =
  max 0 (fromIntegral b - fromIntegral a)

monoPenaltyInc :: Word -> Word -> Int
monoPenaltyInc a b =
  max 0 (fromIntegral a - fromIntegral b)

monoLeftRow :: Board -> Index -> Int
monoLeftRow b row =
  foldl
    (\acc (x1, x2) ->
        acc + monoPenaltyDec (at b.grid (x1, row)) (at b.grid (x2, row))
    )
    0
    [(I0, I1), (I1, I2), (I2, I3)]

monoRightRow :: Board -> Index -> Int
monoRightRow b row =
  foldl
    (\acc (x1, x2) ->
        acc + monoPenaltyInc (at b.grid (x1, row)) (at b.grid (x2, row))
    )
    0
    [(I0, I1), (I1, I2), (I2, I3)]

monoUpCol :: Board -> Index -> Int
monoUpCol Board {rng=r, grid=g} =
  monoLeftRow (Board {rng=r, grid=transpose g})

monoDownCol :: Board -> Index -> Int
monoDownCol Board {rng=r, grid=g} =
  monoRightRow (Board {rng=r, grid=transpose g})

