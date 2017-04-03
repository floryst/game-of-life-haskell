# Conway's Game Of Life: Haskell Edition

This is the classic Conway's Game of Life, written in Haskell.

## Building and Running

I personally used Haskell [stack](https://docs.haskellstack.org/en/stable/README/).
You will also need SDL2, which can be installed from [these instructions](https://github.com/haskell-game/sdl2/blob/master/README.md).

Once you have all of these prerequisites, building and running is as follows:
1. `stack build`
1. `stack exec gameoflife`

You should get a window with a white background! Now go see the controls below.

## Controls
- `Left mouse click` on the board will toggle a cell
- `Space` will toggle paused state
  - Game is initially in paused state
- Keyboard numbers `1` to `6` will control the time step speed.
`1` is the fastest, `6` is the slowest.
