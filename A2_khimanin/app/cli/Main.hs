module Main (main) where

import A2.Board (Board (..), SlideDirection (..))
import A2.Game (Game (..), Message (..), Player (..), initGame)
import qualified A2.Game as Game
import A2.Quadruple (Quadruple (..))
import Graphics.Vty
  ( Color,
    Event (EvKey),
    Image,
    Key (KChar, KDown, KEsc, KLeft, KRight, KUp),
    Vty (nextEvent, shutdown, update),
    bold,
    brightBlack,
    currentAttr,
    defAttr,
    defaultConfig,
    green,
    horizCat,
    imageWidth,
    pad,
    picForImage,
    red,
    rgbColor,
    string,
    vertCat,
    white,
    withBackColor,
    withForeColor,
    withStyle,
    yellow,
  )
import Graphics.Vty.CrossPlatform (mkVty)
import System.Random (mkStdGen)

tileWidth :: Int
tileWidth = 10

main :: IO ()
main = do
  vty <- mkVty defaultConfig
  let gen = mkStdGen 42
      startState = initGame gen

  run vty startState
  shutdown vty

-- -----------------------------------------------------------------------------
-- Logic
-- -----------------------------------------------------------------------------

run :: Vty -> Game -> IO ()
run vty g = do
  let pic = picForImage (drawGame g)
  update vty pic

  case g of
    Quitted -> return ()
    GameOver _ -> do
      e <- nextEvent vty
      case handleEvent e of
        Just Quit -> return ()
        Just Restart -> run vty (Game.update Restart g)
        _ -> run vty g
    Playing _ _ -> do
      e <- nextEvent vty
      case handleEvent e of
        Just Quit -> return ()
        Just msg -> run vty (Game.update msg g)
        Nothing -> run vty g

handleEvent :: Event -> Maybe Message
handleEvent (EvKey KUp []) = Just $ Slide SUp
handleEvent (EvKey KDown []) = Just $ Slide SDown
handleEvent (EvKey KLeft []) = Just $ Slide SLeft
handleEvent (EvKey KRight []) = Just $ Slide SRight
handleEvent (EvKey (KChar 'w') []) = Just $ Slide SUp
handleEvent (EvKey (KChar 's') []) = Just $ Slide SDown
handleEvent (EvKey (KChar 'a') []) = Just $ Slide SLeft
handleEvent (EvKey (KChar 'd') []) = Just $ Slide SRight
handleEvent (EvKey (KChar 'r') []) = Just Restart
handleEvent (EvKey (KChar 'q') []) = Just Quit
handleEvent (EvKey (KChar 'b') []) = Just ToggleBot
handleEvent (EvKey (KChar 'n') []) = Just BotStep
handleEvent (EvKey KEsc []) = Just Quit
handleEvent _ = Nothing

-- -----------------------------------------------------------------------------
-- Rendering
-- -----------------------------------------------------------------------------

drawGame :: Game -> Image
drawGame (Playing pk b) =
  let (modeColor, botKey) = case pk of
        Human -> (green, "")
        Bot _ -> (yellow, "(n) Step Bot")
   in padScreen $
        vertCat
          [ alignCenter $ string (defAttr `withStyle` bold) "2048",
            pad 0 0 0 1 $ alignCenter $ string (defAttr `withForeColor` modeColor) $ show pk,
            drawBoard b,
            pad 0 1 0 0 $ alignCenter $ string (defAttr `withForeColor` brightBlack) "Arrows/WASD to move",
            alignCenter $
              vertCat $
                map
                  (string (defAttr `withForeColor` brightBlack))
                  [ "(b) Toggle Bot  " ++ botKey,
                    "(r) Restart     (q) Quit"
                  ]
          ]
drawGame (GameOver b) =
  padScreen $
    vertCat
      [ alignCenter $ string (defAttr `withForeColor` red `withStyle` bold) "GAME OVER!",
        drawBoard b,
        pad 0 2 0 0 $
          alignCenter $
            vertCat
              [ string defAttr "(r) Try Again",
                string defAttr "(q) Quit"
              ]
      ]
drawGame Quitted = padScreen $ string defAttr "Bye!"

alignCenter :: Image -> Image
alignCenter img =
  let boardW = tileWidth * 4
      padding = max 0 (boardW - imageWidth img) `div` 2
   in pad padding 0 0 0 img

padScreen :: Image -> Image
padScreen = pad 4 2 0 0

drawBoard :: Board -> Image
drawBoard bs =
  let rows = grid bs
      imgRows = foldr (\row acc -> drawRow row : acc) [] rows
   in vertCat imgRows

drawRow :: Quadruple Word -> Image
drawRow row =
  let cells = foldr (\val acc -> drawTile val : acc) [] row
   in horizCat cells

drawTile :: Word -> Image
drawTile val =
  let bgColor = tileColour val
      attr = currentAttr `withBackColor` bgColor `withForeColor` white `withStyle` bold

      txt = if val == 0 then "" else show val
      len = length txt
      padLeft = (tileWidth - len) `div` 2
      padRight = tileWidth - len - padLeft

      emptyRowStr = replicate tileWidth ' '
      textRowStr = replicate padLeft ' ' ++ txt ++ replicate padRight ' '

      top = string attr emptyRowStr
      middle = string attr textRowStr
      bottom = string attr emptyRowStr
   in vertCat [top, middle, bottom]

-- | Adapted from <https://github.com/gabrielecirulli/2048/blob/master/style/main.css>
tileColour :: Word -> Color
tileColour 0 = rgbc 60 63 65
tileColour 2 = rgbc 238 228 218 -- #eee4da
tileColour 4 = rgbc 237 224 200 -- #ede0c8
tileColour 8 = rgbc 242 177 121 -- #f2b179
tileColour 16 = rgbc 245 149 99 -- #f59563
tileColour 32 = rgbc 246 124 95 -- #f67c5f
tileColour 64 = rgbc 246 94 59 -- #f65e3b
tileColour 128 = rgbc 237 207 114 -- #edcf72
tileColour 256 = rgbc 237 204 97 -- #edcc61
tileColour 512 = rgbc 237 200 80 -- #edc850
tileColour 1024 = rgbc 237 197 63 -- #edc53f
tileColour 2048 = rgbc 237 194 46 -- #edc22e
tileColour _ = tileColour 2048

rgbc :: Word -> Word -> Word -> Color
rgbc = rgbColor
