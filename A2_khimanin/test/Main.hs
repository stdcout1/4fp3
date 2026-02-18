{-# LANGUAGE BlockArguments #-}

-- | See https://hackage-content.haskell.org/package/tasty-hunit-0.10.2/docs/Test-Tasty-HUnit.html
-- for documentation on writing unit tests.
module Main where

import A2.Board
import A2.Bot
import A2.Game
import A2.Grid4x4 hiding (update)
import qualified A2.Grid4x4 as A2
import A2.Quadruple
import System.Random (StdGen, mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit

sampleQuad :: Quadruple Int
sampleQuad = Quad 1 2 3 4

quadrupleTests :: TestTree
quadrupleTests =
  testGroup
    "Quadruple"
    [ testCase "fmap" do
        fmap (+ 1) sampleQuad
          @?= Quad 2 3 4 5,
      testCase "foldr" do
        foldr (+) 0 (pure 1 :: Quadruple Int)
          @?= 4,
      testCase "pure" do
        (pure 1 :: Quadruple Int)
          @?= Quad 1 1 1 1,
      testCase "getQuad" do
        getQuad I0 sampleQuad
          @?= 1,
      testCase "setQuad" do
        setQuad I1 1 sampleQuad
          @?= Quad 1 1 3 4,
      testCase "reverseQuad" do
        reverseQuad sampleQuad
          @?= Quad 4 3 2 1
          -- we dont need to test indices...
    ]

sampleGrid :: Grid4x4 Int
sampleGrid = Quad sampleQuad sampleQuad sampleQuad sampleQuad

grid4x4Tests :: TestTree
grid4x4Tests =
  testGroup
    "Grid4x4"
    [ -- Your tests here!
      testCase "at" do
        at sampleGrid (I0, I0)
          @?= 1,
      testCase "update" do
        A2.update (I0, I0) 32 sampleGrid
          @?= Quad (Quad 32 2 3 4) sampleQuad sampleQuad sampleQuad,
      testCase "allPairs" do
        A2.indices
          @?= [(I0, I0), (I0, I1), (I0, I2), (I0, I3), (I1, I0), (I1, I1), (I1, I2), (I1, I3), (I2, I0), (I2, I1), (I2, I2), (I2, I3), (I3, I0), (I3, I1), (I3, I2), (I3, I3)],
      testCase "transpose" do
        let original = (Quad sampleQuad (Quad 5 6 7 8) (Quad 5 6 7 8) sampleQuad)
         in at (transpose original) (I0, I3)
              @?= at original (I3, I0)
    ]

testGen :: StdGen
testGen = mkStdGen 47

boardTests :: TestTree
boardTests =
  testGroup
    "Board"
    [ testCase "twoDistinctRandomElem" do
        case (twoDistinctRandomElem testGen [0 .. 10 :: Int]) of
          (4, 8, _) -> pure ()
          _ -> assertBool "wrong elements selected" False,
      -- testing two functions at the same time !!!!!!
      testCase "freshBoard has 14 empty spots" do
        let b = freshBoard testGen
        length (emptySpots b) @?= 14,
      testCase "fresh board is not game over" do
        let b = freshBoard testGen
        isGameOver b @?= False,
      testCase "pushRight zero" $
        pushRowRight (Quad 0 0 0 0) @?= Quad 0 0 0 0,
      testCase "pushRight pack" $
        pushRowRight (Quad 2 0 4 0) @?= Quad 0 0 2 4,
      testCase "mergeRight end" $
        mergeRowRight (Quad 0 0 2 2) @?= Quad 0 0 0 4,
      testCase "mergeRight mid" $
        mergeRowRight (Quad 0 2 2 4) @?= Quad 0 0 4 4,
      testCase "slideRight simple" $
        slideRowRight (Quad 2 0 2 0) @?= Quad 0 0 0 4,
      testCase "slideRight triple" $
        slideRowRight (Quad 2 2 2 0) @?= Quad 0 0 2 4,
      testCase "slideLeft simple" $
        slideRowLeft (Quad 0 2 0 2) @?= Quad 4 0 0 0,
      testCase "slideLeft triple" $
        slideRowLeft (Quad 0 2 2 2) @?= Quad 4 2 0 0,
      testCase "slideRight triple" $
        slideRowRight (Quad 2 2 2 0) @?= Quad 0 0 2 4,
      testCase "slideRight doublepair" $
        slideRowRight (Quad 2 2 4 4) @?= Quad 0 0 4 8,
      testCase "slideLeft doublepair" $
        slideRowLeft (Quad 2 2 4 4) @?= Quad 4 8 0 0,
      testCase "slideRight allsame" $
        slideRowRight (Quad 2 2 2 2) @?= Quad 0 0 4 4,
      testCase "slideLeft allsame" $
        slideRowLeft (Quad 2 2 2 2) @?= Quad 4 4 0 0,
      testCase "slideRight nochain" $
        slideRowRight (Quad 4 2 2 2) @?= Quad 0 4 2 4,
      testCase "slideLeft nochain" $
        slideRowLeft (Quad 2 2 2 4) @?= Quad 4 2 4 0,
      testCase "grid left" $
        slideGrid
          SLeft
          ( Quad
              (Quad 0 2 0 2)
              (Quad 2 0 0 2)
              (Quad 0 0 0 0)
              (Quad 4 4 2 0)
          )
          @?= ( Quad
                  (Quad 4 0 0 0)
                  (Quad 4 0 0 0)
                  (Quad 0 0 0 0)
                  (Quad 8 2 0 0)
              ),
      testCase "grid right" $
        slideGrid
          SRight
          ( Quad
              (Quad 0 2 0 2)
              (Quad 2 0 0 2)
              (Quad 0 0 0 0)
              (Quad 4 4 2 0)
          )
          @?= ( Quad
                  (Quad 0 0 0 4)
                  (Quad 0 0 0 4)
                  (Quad 0 0 0 0)
                  (Quad 0 0 8 2)
              ),
      testCase "grid up" $ do
        let g =
              Quad
                (Quad 2 0 0 0)
                (Quad 2 0 0 0)
                (Quad 0 0 0 0)
                (Quad 0 0 0 0)
        slideGrid SUp g
          @?= transpose (slideGrid SLeft (transpose g)),
      testCase "grid down" $ do
        let g =
              Quad
                (Quad 2 0 0 0)
                (Quad 2 0 0 0)
                (Quad 0 0 0 0)
                (Quad 0 0 0 0)
        slideGrid SDown g
          @?= transpose (slideGrid SRight (transpose g))
    ]

gameTests :: TestTree
gameTests =
  testGroup
    "Game"
    [ testCase "init playing" do
        case initGame testGen of
          Playing Human _ -> pure ()
          _ -> assertBool "expected Playing Human" False,
      testCase "init empties" do
        case initGame testGen of
          Playing Human b -> length (emptySpots b) @?= 14
          _ -> assertBool "expected Playing Human" False,
      testCase "toggle bot" do
        let g0 = initGame testGen
            g1 = update ToggleBot g0
        case (g0, g1) of
          (Playing Human _, Playing (Bot _) _) -> pure ()
          _ -> assertBool "expected Human -> Bot" False,
      testCase "toggle human" do
        let g0 = initGame testGen
            g1 = update ToggleBot g0
            g2 = update ToggleBot g1
        case g2 of
          Playing Human _ -> pure ()
          _ -> assertBool "expected Bot -> Human" False,
      testCase "quit" do
        update Quit (initGame testGen) @?= Quitted,
      testCase "restart playing" do
        let g0 = initGame testGen
            g1 = update (Slide SLeft) g0
            g2 = update Restart g1
        case g2 of
          Playing Human b -> length (emptySpots b) @?= 14
          _ -> assertBool "expected Playing Human" False,
      testCase "slide keeps state" do
        let g0 = initGame testGen
            g1 = update (Slide SLeft) g0
        case g1 of
          Playing Human _ -> pure ()
          _ -> assertBool "expected still Playing Human" False,
      testCase "restart gameover" do
        case initGame testGen of
          Playing Human b ->
            case update Restart (GameOver b) of
              Playing Human b2 -> length (emptySpots b2) @?= 14
              _ -> assertBool "expected Playing Human after restart" False
          _ -> assertBool "expected Playing Human" False,
      testCase "ignored msg" do
        -- begin is being ignored so it shouldnt change the game
        let g0 = initGame testGen
            g1 = update (Begin testGen) g0
        g1 @?= g0
    ]

testBoard :: Grid -> Board
testBoard g =
  Board { grid = g, rng = testGen }

botTests :: TestTree
botTests =
  testGroup
    "Bot"
    [ testCase "emptyRow" do
        let b =
              testBoard $
                Quad
                  (Quad 0 2 0 4)
                  (Quad 0 0 0 0)
                  (Quad 2 2 2 2)
                  (Quad 4 0 4 0)

        emptyCellsInRow b I0 @?= 2
        emptyCellsInRow b I1 @?= 4
        emptyCellsInRow b I2 @?= 0
        emptyCellsInRow b I3 @?= 2,
      testCase "emptyCol" do
        let b =
              testBoard $
                Quad
                  (Quad 0 2 0 4)
                  (Quad 0 0 0 0)
                  (Quad 2 2 2 2)
                  (Quad 4 0 4 0)

        emptyCellsInCol b I0 @?= 2
        emptyCellsInCol b I1 @?= 2
        emptyCellsInCol b I2 @?= 2
        emptyCellsInCol b I3 @?= 2,
      testCase "mergeRow" do
        let b =
              testBoard $
                Quad
                  (Quad 2 2 4 4) -- two adjacent merges
                  (Quad 2 0 2 2) -- one merge (last pair)
                  (Quad 0 0 0 0)
                  (Quad 8 8 8 0) -- one merge
        mergesInRow b I0 @?= 2
        mergesInRow b I1 @?= 1
        mergesInRow b I2 @?= 0
        mergesInRow b I3 @?= 1,
      testCase "mergeCol" do
        let b =
              testBoard $
                Quad
                  (Quad 2 0 4 0)
                  (Quad 2 0 4 0)
                  (Quad 0 0 4 0)
                  (Quad 0 0 0 0)

        mergesInCol b I0 @?= 1 -- (2,2,0,0)
        mergesInCol b I2 @?= 1, -- (4,4,4,0)
      testCase "sumRow" do
        let b =
              testBoard $
                Quad
                  (Quad 0 2 0 4)
                  (Quad 1 1 1 1)
                  (Quad 2 2 2 2)
                  (Quad 4 0 4 0)

        sumInRow b I0 @?= 6
        sumInRow b I1 @?= 4
        sumInRow b I2 @?= 8
        sumInRow b I3 @?= 8,
      testCase "sumCol" do
        let b =
              testBoard $
                Quad
                  (Quad 0 2 0 4)
                  (Quad 1 1 1 1)
                  (Quad 2 2 2 2)
                  (Quad 4 0 4 0)

        sumInCol b I0 @?= 7 -- 0+1+2+4
        sumInCol b I1 @?= 5
        sumInCol b I3 @?= 7,
      testCase "monoRow" do
        let b =
              testBoard $
                Quad
                  (Quad 8 4 2 0) -- monotone decreasing
                  (Quad 0 2 4 8) -- monotone increasing
                  (Quad 2 8 4 1) -- mixed
                  (Quad 2 2 2 2) -- flat
        monoLeftRow b I0 @?= 0
        monoRightRow b I0 @?= 8

        monoRightRow b I1 @?= 0
        monoLeftRow b I1 @?= 8

        monoLeftRow b I3 @?= 0
        monoRightRow b I3 @?= 0,
      testCase "monoCol" do
        let b =
              testBoard $
                Quad
                  (Quad 8 0 0 0)
                  (Quad 4 0 0 0)
                  (Quad 2 0 0 0)
                  (Quad 0 0 0 0)

        monoUpCol b I0 @?= 0
        monoDownCol b I0 @?= 8
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "A2"
      [ quadrupleTests,
        grid4x4Tests,
        boardTests,
        gameTests,
        botTests
      ]
