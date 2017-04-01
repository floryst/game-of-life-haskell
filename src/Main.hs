{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import SDL.Vect
import SDL.Event
import Data.Maybe
import Control.Monad (unless)

data GameState = GameState {
  isPaused :: Bool,
  shouldQuit :: Bool
} deriving (Show)

-- was a specific key pressed
keyPressed :: SDL.Keycode -> SDL.EventPayload -> Bool
keyPressed keycode (SDL.KeyboardEvent keyboardEvent) =
  SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
  SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keycode
keyPressed keycode _ = False

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Game Of Life" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  let gameState = GameState {
    isPaused = False,
    shouldQuit = False
  }

  -- main loop
  appLoop gameState renderer

  -- teardown resources
  SDL.destroyWindow window
  SDL.quit

handleKeyEvent :: GameState -> SDL.KeyboardEventData -> GameState
handleKeyEvent gameState SDL.KeyboardEventData {
  keyboardEventWindow = _,
  keyboardEventKeyMotion = SDL.Pressed,
  keyboardEventRepeat = False,
  keyboardEventKeysym = SDL.Keysym {
    -- needs SDL.* qualifier since these symbols aren't imported to
    -- primary scope.
    SDL.keysymScancode = _,
    SDL.keysymKeycode = keyCode,
    SDL.keysymModifier = _
  }
} =
  case keyCode of
    SDL.KeycodeQ ->
      gameState { shouldQuit = True }
    -- Do I really need this?
    _ ->
      gameState

updateState :: GameState -> Maybe SDL.Event -> IO GameState
updateState gameState event = do
  case event of
    Just ev -> do
      case SDL.eventPayload ev of
        SDL.QuitEvent -> do
          return gameState { shouldQuit = True }
        SDL.KeyboardEvent keyEvent -> do
          return (handleKeyEvent gameState keyEvent)
        -- Do I really need this?
        _ -> do
          return gameState
    Nothing -> do
      return gameState

appLoop :: GameState -> SDL.Renderer -> IO ()
appLoop gameState renderer = do
  event <- SDL.pollEvent
  let white = V4 255 255 255 255
      black = V4 0 0 0 255

  gameState <- updateState gameState event

  SDL.rendererDrawColor renderer SDL.$= white
  SDL.clear renderer

  SDL.rendererDrawColor renderer SDL.$= black
  let rect = Just $ SDL.Rectangle (SDL.P (V2 1 1)) (V2 40 40)
  SDL.fillRect renderer rect

  SDL.present renderer
  unless (shouldQuit gameState) (appLoop gameState renderer)
