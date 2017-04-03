{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import SDL.Vect
import SDL.Event
import Data.Maybe
import Control.Monad (unless, guard)
import Foreign.C.Types
import Debug.Trace
import Data.Vector ((!))
import Data.List (elemIndex)
import Data.Word (Word32)
import qualified Data.Vector as Vector

type CellGrid = Vector.Vector (Vector.Vector Bool)

data GameState = GameState {
    isPaused :: Bool,
    shouldQuit :: Bool,
    grid :: CellGrid,
    lastUpdateTime :: Int,
    stepSpeed :: Int
} deriving (Show)

windowWidth, windowHeight :: CInt
(windowWidth, windowHeight) = (900, 900)

-- in milliseconds
gameTimeSteps :: [Int]
gameTimeSteps = [10, 50, 100, 200, 500, 1000]

gameTimeStepKeys :: [SDL.Keycode]
gameTimeStepKeys = [SDL.Keycode1,
                    SDL.Keycode2,
                    SDL.Keycode3,
                    SDL.Keycode4,
                    SDL.Keycode5,
                    SDL.Keycode6]

-- cell dimensions must be a square, and a factor of window width/height
-- that is, window width/height must be a multiple of cell side length
cellSideLength :: Int
cellSideLength = 20

-- converts from coords in grid space to coords in window space
gridToWindowCoord :: (Int, Int) -> (Int, Int)
gridToWindowCoord (x, y) = (x * cellSideLength, y * cellSideLength)

-- converts from window space to grid space
windowToGridCoord :: (Int, Int) -> (Int, Int)
windowToGridCoord (x, y) = (x `quot` cellSideLength, y `quot` cellSideLength)

-- generates initial grid
initialGrid :: CellGrid
initialGrid = Vector.replicate height . Vector.replicate width $ False
  where
    height = (fromIntegral windowHeight) `quot` cellSideLength
    width = (fromIntegral windowWidth) `quot` cellSideLength

-- access grid as if on a torus
gridAt :: CellGrid -> (Int, Int) -> Bool
gridAt cellGrid (x, y) = (cellGrid!(y `mod` height))!(x `mod` width)
  where
    height = length cellGrid
    -- grid rows should all be same length
    width = length $ cellGrid!1

-- toggle state of cell at grid coord (x, y)
toggleCellState :: (Int, Int) -> CellGrid -> CellGrid
toggleCellState targetCoord cellGrid = Vector.fromList $ do
    (y, row) <- enumerate $ Vector.toList cellGrid
    return $ Vector.fromList $ do
        (x, cell) <- enumerate $ Vector.toList row
        return $
            if targetCoord == (x, y)
                then not cell
                else cell
  where
    enumerate = zip [0..]

-- get coords (in grid space) of alive cells
getAliveCellCoords :: CellGrid  -> [(Int, Int)]
getAliveCellCoords cellGrid = do
    (y, row) <- enumerate $ Vector.toList cellGrid
    (x, cell) <- enumerate $ Vector.toList row
    guard cell
    return (x, y)
  where
    enumerate = zip [0..]

liveNeighbors :: Int -> Int -> CellGrid -> Int
liveNeighbors x y cellGrid = sum $ map fromEnum neighbors
  where
    coordSum (a, b) (c, d) = (a+c, b+d)
    neighborLocations =
        [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    neighbors = map (gridAt cellGrid . coordSum (x, y)) neighborLocations


stepGameOfLife :: CellGrid -> CellGrid
stepGameOfLife cellGrid = newGrid
  where
    gameRule cell neighs = (neighs >= 2 && neighs < 4 && cell) || neighs == 3
    -- for every row,
    newGrid = Vector.map updateRow $ Vector.indexed cellGrid
    -- for every cell,
    updateRow (y, row) = Vector.map (updateCell y) $ Vector.indexed row
    -- update cell based on rules
    updateCell y (x, cell) = gameRule cell $ liveNeighbors x y cellGrid

--
-- Event handling
--

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
        SDL.KeycodeQ -> gameState { shouldQuit = True }
        SDL.KeycodeSpace -> gameState { isPaused = not $ isPaused gameState }
        keyCode | keyCode `elem` gameTimeStepKeys ->
                    gameState {
                        stepSpeed = gameTimeSteps !! fromMaybe 0 (elemIndex keyCode gameTimeStepKeys)
                    }
                | otherwise -> gameState
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
    mouseButtonEventPos = SDL.P (V2 clickX clickY)
} =
    let newGrid = toggleCellState (windowToGridCoord (winX, winY)) $ cellGrid
      in
        gameState { grid = newGrid }
  where
    cellGrid = grid gameState
    winX = fromIntegral clickX
    winY = fromIntegral clickY
handleMouseEvent gameState _ = gameState

-- updates game state based on events
processEvents :: GameState -> Maybe SDL.Event -> IO GameState
processEvents gameState event = do
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

--
-- Drawing
--

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
drawGrid :: CellGrid -> SDL.Renderer-> IO ()
drawGrid cellGrid renderer = do
    -- colors
    let white = V4 255 255 255 255
        black = V4 0 0 0 255

    SDL.rendererDrawColor renderer SDL.$= white
    SDL.clear renderer

    SDL.rendererDrawColor renderer SDL.$= black
    let aliveCells = map gridToWindowCoord $ getAliveCellCoords $ cellGrid
      in
        mapM_ (drawCell renderer) aliveCells
    SDL.present renderer

--
-- Main
--

-- main app loop
appLoop :: GameState -> SDL.Renderer -> IO ()
appLoop gameState renderer = do
    event <- SDL.pollEvent
    gameState <- processEvents gameState event
    drawGrid (grid gameState) renderer

    t <- SDL.ticks
    -- must be different name than variable 't'. Otherwise, loop will break.
    let ticks = fromIntegral t
    gameState <- do
        if not $ isPaused gameState
            then if (lastUpdateTime gameState) + (stepSpeed gameState) < ticks
                then return gameState {
                    grid = stepGameOfLife $ grid gameState,
                    lastUpdateTime = ticks
                }
                else return gameState
            else return gameState

    -- should I use threadDelay instead? *shrugs*
    SDL.delay 10
    unless (shouldQuit gameState) (appLoop gameState renderer)

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Game Of Life" SDL.defaultWindow {
        SDL.windowInitialSize = V2 windowWidth windowHeight
    }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    let gameState = GameState {
        -- don't evolve just yet
        isPaused = True,
        shouldQuit = False,
        grid = initialGrid,
        lastUpdateTime = 0,
        stepSpeed = 100
    }

    -- main loop
    appLoop gameState renderer

    -- teardown resources
    SDL.destroyWindow window
    SDL.quit
