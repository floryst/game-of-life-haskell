{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import SDL.Vect
import SDL.Event
import Data.Maybe
import Control.Monad (unless, guard)
import Foreign.C.Types
import Debug.Trace

data GameState = GameState {
    isPaused :: Bool,
    shouldQuit :: Bool,
    grid :: [[Bool]]
} deriving (Show)

windowWidth, windowHeight :: CInt
(windowWidth, windowHeight) = (900, 900)

-- cell dimensions must be a square, and a factor of window width/height
-- that is, window width/height must be a multiple of cell side length
cellSideLength :: Int
cellSideLength = 10

-- converts from coords in grid space to coords in window space
gridToWindowCoord :: (Int, Int) -> (Int, Int)
gridToWindowCoord (x, y) = (x * cellSideLength, y * cellSideLength)

-- converts from window space to grid space
windowToGridCoord :: (Int, Int) -> (Int, Int)
windowToGridCoord (x, y) = (x `quot` cellSideLength, y `quot` cellSideLength)

initialGrid :: [[Bool]]
initialGrid = replicate height . replicate width $ False
  where
    height = (fromIntegral windowHeight) `quot` cellSideLength
    width = (fromIntegral windowWidth) `quot` cellSideLength

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

-- toggle state of cell at grid coord (x, y)
toggleCellState :: (Int, Int) -> GameState -> GameState
toggleCellState targetCoord gameState = gameState { grid = newGrid }
  where
    newGrid = do
        (y, row) <- enumerate cellGrid
        return $ do
            (x, cell) <- enumerate row
            return $
                if targetCoord == (x, y)
                    then not cell
                    else cell
      where
        cellGrid = grid gameState
        enumerate = zip [0..]


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
    mouseButtonEventPos = SDL.P (V2 clickX clickY)
} =
    toggleCellState (windowToGridCoord (winX, winY)) gameState
  where
    winX = fromIntegral clickX
    winY = fromIntegral clickY
handleMouseEvent gameState _ = gameState

-- updates game state based on events
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

-- get coords (in grid space) of alive cells
getAliveCellCoords :: GameState -> [(Int, Int)]
getAliveCellCoords gameState = do
    (y, row) <- enumerate cellGrid
    (x, cell) <- enumerate row
    guard cell
    return (x, y)
  where
    cellGrid = grid gameState
    enumerate = zip [0..]

-- draws a cell at window coords
drawCell :: SDL.Renderer -> (Int, Int) -> IO ()
drawCell renderer (x, y) =
    -- SDL.P creates a point
    -- Docs: https://hackage.haskell.org/package/sdl2-2.2.0/docs/SDL-Vect.html#t:Point
    SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ V2 cx cy) (V2 len len)
  where
    -- square side length
    len  = fromIntegral cellSideLength
    -- CInt x and y
    cx = fromIntegral x
    cy = fromIntegral y

-- draws grid
drawGrid :: GameState -> SDL.Renderer-> IO ()
drawGrid gameState renderer = do
    -- colors
    let white = V4 255 255 255 255
        black = V4 0 0 0 255

    SDL.rendererDrawColor renderer SDL.$= white
    SDL.clear renderer

    SDL.rendererDrawColor renderer SDL.$= black
    let aliveCells = map gridToWindowCoord $ getAliveCellCoords gameState
      in
        mapM_ (drawCell renderer) aliveCells
    SDL.present renderer

appLoop :: GameState -> SDL.Renderer -> IO ()
appLoop gameState renderer = do
    event <- SDL.pollEvent
    gameState <- updateState gameState event
    drawGrid gameState renderer
    unless (shouldQuit gameState) (appLoop gameState renderer)
