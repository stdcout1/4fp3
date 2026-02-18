{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module A2.Game
  ( Game (..),
    initGame,
    Message (..),
    update,
    Player (..),
  )
where

import A2.Board
  ( Board,
    SlideDirection (..),
    freshBoard,
    isGameOver,
    refreshBoard,
    slide,
  )
import A2.Bot (MyBot, botInfoMsg, initBot, nextMove)
import System.Random (StdGen)

-- | The active player entity. Either a Human or a Bot (with an internal
-- memory).
data Player where
  Human :: Player
  Bot :: MyBot -> Player
  deriving (Eq)

instance Show Player where
  show Human = "Human"
  show (Bot t) = "Bot (" ++ botInfoMsg t ++ ")"

-- | A 2048 game has 3 possible states: playing, waiting for restart, and
-- shutting down.
data Game
  = Playing Player Board
  | GameOver Board
  | Quitted
  deriving (Show, Eq)

-- | Initialize a 2048 game.
initGame :: StdGen -> Game
initGame = Playing Human . freshBoard

-- | All possible actions a player (or bot) can make in a game of 2048.
data Message
  = Begin StdGen
  | Slide SlideDirection
  | ToggleBot
  | BotStep
  | Restart
  | Quit
  deriving (Show)


-- | Updates the game model according to actions a user (or bot) make.
update :: Message -> Game -> Game
update ToggleBot (Playing Human b) = Playing (Bot (initBot b)) b
update ToggleBot (Playing (Bot _) b) = Playing Human b
update BotStep (Playing (Bot bot) b) =
    let 
    -- calculate the next move and update to board 
    -- to reflect it
        (newBot, dir) = nextMove b bot
        (newBoard, didSomething) = slide b dir

    in
    if didSomething then 
        Playing (Bot newBot) newBoard
    else 
        -- if the move didnt do anything we should
        if isGameOver b then GameOver b else Playing (Bot newBot) newBoard
update (Slide dir) (Playing Human b) = 
    let 
    -- calculate the next move and update to board 
    -- to reflect it
        (newBoard, didSomething) = slide b dir
    in
    if didSomething then 
        Playing Human newBoard
    else 
        -- if the move didnt do anything we should
        if isGameOver b then GameOver b else Playing Human newBoard
update Restart (Playing _ b) = Playing Human (refreshBoard b)
update Restart (GameOver b) = Playing Human (refreshBoard b)
update Quit _ = Quitted
update _ g = g
