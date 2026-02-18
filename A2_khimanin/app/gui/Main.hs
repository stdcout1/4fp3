{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where

import A2.Board (Board (..), SlideDirection (..))
import A2.Game (Game (..), Message (..), Player (..), initGame, update)
import A2.Grid4x4 (at, indices)
import A2.Quadruple (Index (..), Quadruple (..))
import Brillo
  ( Color,
    Display (InWindow),
    Picture,
    black,
    blank,
    color,
    makeColorI,
    pictures,
    rectangleSolid,
    scale,
    text,
    translate,
    white,
  )
import Brillo.Interface.IO.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down),
    SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp),
    playIO,
  )
import System.Exit (exitSuccess)
import System.Random (mkStdGen)

windowWidth, windowHeight :: Int
windowWidth = 600
windowHeight = 800

bgColour :: Color
bgColour = makeColorI 250 248 239 255

main :: IO ()
main =
  playIO
    (InWindow "2048" (windowWidth, windowHeight) (100, 100))
    bgColour -- Background Colour
    10 -- FPS (Keep low, else bot goes too fast)
    initialGame -- Initial Game
    drawGame -- Render
    handleInput -- Input Handler
    stepGame -- Time Step (Handles bot auto-step)

initialGame :: Game
initialGame = initGame (mkStdGen 42)

-- While testing, using a fixed seed for the random number generator (StdGen) is
-- useful, e.g., `mkStdGen 42`. However, you should also test using the global
-- random number generator, `getStdGen`.

-- -----------------------------------------------------------------------------
-- Logic
-- -----------------------------------------------------------------------------

-- | Bot operates on each frame render.
stepGame :: Float -> Game -> IO Game
stepGame _ g@(Playing (Bot _) _) = return $ update BotStep g
stepGame _ g = return g

-- | Updates the game state given input events.
handleInput :: Event -> Game -> IO Game
handleInput (EventKey (Char 'q') Down _ _) _ = exitSuccess
handleInput (EventKey (SpecialKey KeyUp) Down _ _) g = return $ update (Slide SUp) g
handleInput (EventKey (SpecialKey KeyDown) Down _ _) g = return $ update (Slide SDown) g
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) g = return $ update (Slide SLeft) g
handleInput (EventKey (SpecialKey KeyRight) Down _ _) g = return $ update (Slide SRight) g
handleInput (EventKey (Char 'r') Down _ _) g = return $ update Restart g
handleInput (EventKey (Char 'b') Down _ _) g = return $ update ToggleBot g
handleInput (EventKey (Char 'n') Down _ _) g = return $ update BotStep g
handleInput _ g = return g

-- -----------------------------------------------------------------------------
-- Rendering
-- -----------------------------------------------------------------------------


-- some constants to help with sizing. works well on my weird 3:2 (2256 x 1504) screen...
tileSize, tileGap :: Float
tileSize = 110
tileGap = 12

boardDim :: Int
boardDim = 4

boardW, boardH :: Float
boardW = fromIntegral boardDim * tileSize + fromIntegral (boardDim + 1) * tileGap
boardH = boardW

boardCenterY :: Float
boardCenterY = 60

uiGrey :: Color
uiGrey = makeColorI 120 110 100 255

titleColour :: Color
titleColour = makeColorI 119 110 101 255

red :: Color
red = makeColorI 220 60 60 255


-- function to help with text placement in the middle. 
-- i tried to make it calculate x based on length but it was kinda wonkey...
txt :: Float -> Color -> Float -> String -> Picture
txt s col y str =
  translate 0 y $
    scale s s $
      color col $
        text str

drawBoardPic :: Board -> Picture
drawBoardPic bs =
    -- first setup a background 
  let tray =
        color (makeColorI 187 173 160 255) $
          rectangleSolid boardW boardH
      --  then fold through the grid drawing each row. 
      --  the accunaltor has a row counter to offset the row position on screen 
      tiles =
      --  obviously we only need to final accunulated picture, so we drop 
      --  the r
        snd $
          foldl
            ( \(r, acc) row ->
                (r + 1, drawRowAt r row ++ acc)
            )
            (0, [])
            bs.grid
    -- then we return a picture which is [tray(bg), tile1, tile2] 
   in pictures (tray : tiles)

drawRowAt :: Int -> Quadruple Word -> [Picture]
drawRowAt r row =
  snd $
    foldl
    -- similar idea from above to draw each tile.
      ( \(c, acc) val ->
          (c + 1, drawTileAt r c val : acc)
      )
      (0, [])
      row

drawTileAt :: Int -> Int -> Word -> Picture
drawTileAt r c val =
  -- we use the constants above to draw the tiles
  let leftX = -(boardW / 2) + tileGap + tileSize / 2
      topY = boardH / 2 - tileGap - tileSize / 2

      x = leftX + fromIntegral c * (tileSize + tileGap)
      y = topY - fromIntegral r * (tileSize + tileGap)

      tile =
        translate x y $
      -- fetch the bg color for the value 
          color (tileColour val) $
            rectangleSolid tileSize tileSize

      -- if the value is zero leave it blank else draw the associated value 
      label =
        if val == 0
          then blank
          else
            translate x (y - 12) $
              scale 0.18 0.18 $
                color black $
                  text (show val)
   in pictures [tile, label]

-- | Renders the game given its current state.
drawGame :: Game -> IO Picture
drawGame (Playing p b) =
-- we we are playing. show who is playing the the controls
-- plus obviously the board
  let botKey =
        case p of
          Human -> ""
          Bot _ -> "(n) Step Bot"
      headerY = fromIntegral windowHeight / 2
      helpY1 = -(fromIntegral windowHeight / 2) + 140
      helpY2 = helpY1 - 35
      helpY3 = helpY2 - 35
   in return
        ( pictures
            [ txt 0.35 titleColour headerY "2048",
              txt 0.20 red (headerY - 60) (show p),
              translate 0 boardCenterY (drawBoardPic b),
              txt 0.16 uiGrey helpY1 "Arrows/WASD to move",
              txt 0.16 uiGrey helpY2 ("(b) Toggle Bot  " ++ botKey),
              txt 0.16 uiGrey helpY3 "(r) Restart     (q) Quit"
            ]
        )
drawGame (GameOver b) =
-- then wehen we loose plaster a large you loose sign
  let headerY = fromIntegral windowHeight / 2
   in return
        ( pictures
            [ txt 0.35 titleColour headerY "2048",
              translate 0 boardCenterY (drawBoardPic b),
              translate (-120) 0 (txt 0.5 red (headerY-240) "You LOOSE!")
            ]
        )
-- unreachable
drawGame Quitted = pure blank

-- | Adapted from <https://github.com/gabrielecirulli/2048/blob/master/style/main.css>
tileColour :: Word -> Color
tileColour 0 = makeColorI 205 193 180 255
tileColour 2 = makeColorI 238 228 218 255 -- #eee4da
tileColour 4 = makeColorI 237 224 200 255 -- #ede0c8
tileColour 8 = makeColorI 242 177 121 255 -- #f2b179
tileColour 16 = makeColorI 245 149 99 255 -- #f59563
tileColour 32 = makeColorI 246 124 95 255 -- #f67c5f
tileColour 64 = makeColorI 246 94 59 255 -- #f65e3b
tileColour 128 = makeColorI 237 207 114 255 -- #edcf72
tileColour 256 = makeColorI 237 204 97 255 -- #edcc61
tileColour 512 = makeColorI 237 200 80 255 -- #edc850
tileColour 1024 = makeColorI 237 197 63 255 -- #edc53f
tileColour 2048 = makeColorI 237 194 46 255 -- #edc22e
tileColour _ = tileColour 2048
