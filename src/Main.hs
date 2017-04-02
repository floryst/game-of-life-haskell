{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import SDL.Vect
import SDL.Event
import Data.Maybe
import Control.Monad (unless)
import Foreign.C.Types
import Debug.Trace

data GameState = GameState {
    isPaused :: Bool,
    shouldQuit :: Bool,
    grid :: [Bool]
} deriving (Show)

windowWidth, windowHeight :: CInt
(windowWidth, windowHeight) = (900, 900)

-- cell dimensions must be a square, and a factor of window width/height
cellSideLength :: Int
cellSideLength = 10

initialGrid :: [Bool]
initialGrid =
    replicate ((winWidth `quot` sideLen) * (winHeight `quot` sideLen)) False
  where
    winWidth = fromIntegral windowWidth
    winHeight = fromIntegral windowHeight
    sideLen = cellSideLength

-- NOT USED ANYMORE
-- was a specific key pressed
keyPressed :: SDL.Keycode -> SDL.EventPayload -> Bool
keyPressed keycode (SDL.KeyboardEvent keyboardEvent) =
    SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
    SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keycode
keyPressed keycode _ = False

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Game Of Life" SDL.defaultWindow {
        SDL.windowInitialSize = V2 windowWidth windowHeight
    }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    let gameState = GameState {
        isPaused = False,
        shouldQuit = False,
        grid = initialGrid
    }

    -- main loop
    appLoop gameState renderer

    -- teardown resources
    SDL.destroyWindow window
    SDL.quit

-- Updates game state base on key events
handleKeyEvent :: GameState -> SDL.KeyboardEventData -> GameState
handleKeyEvent gameState SDL.KeyboardEventData {
    keyboardEventWindow = _,
    -- only keydown events
    keyboardEventKeyMotion = SDL.Pressed,
    -- no repeat
    keyboardEventRepeat = False,
    -- needs SDL.* qualifier since these symbols aren't imported to
    -- primary scope.
    keyboardEventKeysym = SDL.Keysym {
        -- don't care about scancode
        SDL.keysymScancode = _,
        SDL.keysymKeycode = keyCode,
        -- don't care about modifiers
        SDL.keysymModifier = _
    }
} =
    case keyCode of
      SDL.KeycodeQ ->
          gameState { shouldQuit = True }
      SDL.KeycodeSpace ->
          gameState { isPaused = not $ isPaused gameState }
      _ ->
          gameState
-- fallthrough
handleKeyEvent gameState _ = gameState

-- Updates game state based on mouse events
handleMouseEvent :: GameState -> SDL.MouseButtonEventData -> GameState
handleMouseEvent gameState SDL.MouseButtonEventData {
    mouseButtonEventWindow = _,
    mouseButtonEventMotion = SDL.Pressed,
    mouseButtonEventWhich = _,
    -- only left button
    mouseButtonEventButton = SDL.ButtonLeft,
    -- only 1 click
    mouseButtonEventClicks = 1,
    mouseButtonEventPos = clickCoord
} =
    Debug.Trace.trace ("click: " ++ show clickCoord) $ gameState
handleMouseEvent gameState _ = gameState


updateState :: GameState -> Maybe SDL.Event -> IO GameState
updateState gameState event = do
    case event of
        Just ev -> do
            case SDL.eventPayload ev of
                SDL.QuitEvent -> do
                    return gameState { shouldQuit = True }
                SDL.KeyboardEvent keyEvent -> do
                    return (handleKeyEvent gameState keyEvent)
                SDL.MouseButtonEvent mouseEvent -> do
                    return (handleMouseEvent gameState mouseEvent)
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

    -- white background
    SDL.rendererDrawColor renderer SDL.$= white
    SDL.clear renderer

    -- black rectangle starting at (1,1) with width/height (40, 40)
    SDL.rendererDrawColor renderer SDL.$= black
    -- SDL.P creates a point
    -- Docs: https://hackage.haskell.org/package/sdl2-2.2.0/docs/SDL-Vect.html#t:Point
    let rect = Just $ SDL.Rectangle (SDL.P $ V2 1 1) (V2 40 40)
    SDL.fillRect renderer rect

    SDL.present renderer
    unless (shouldQuit gameState) (appLoop gameState renderer)
