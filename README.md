# spell-monad

WIP. A 2D terminal platformer where you play as a wizard. Your spells are
written in Haskell.

![A demo of some implemented features.\
\
Shows the effect of running `firebolt (1,0)`, then `firebolt (0,1)` with player\
movement, then `inputTarget >>= firebolt`, and then\
`let f m i = m i >> f m (i + 1) in f (\i -> print (1, i / 10) >> firebolt (1, i / 10)) 1`.\
](demo.gif)

## Running

Clone the repository, then run `cabal run spell-monad-exe` to run the game.
Alternatively, run `cabal run spell-monad-exe -- +RTS -N` to use multiple CPU cores.

The game uses around 2 gigabytes of RAM. This is not a memory leak: all of
this is taken by [`hint`](https://hackage.haskell.org/package/hint-0.9.0.8),
the library used for the Haskell interpreter. I don't know whether the memory
usage can be decreased.

## Controls

| Focus | Keys     | Description                           |
| ----- | -------- | ------------------------------------- |
|       | `CTRL-W` | Toggle REPL focus                     |
|       | `CTRL-T` | Toggle REPL visiblity                 |
|       | `q`      | Quit to main menu                     |
| Game  | arrows   | Player/target movement                |
| Game  | `Space`  | Jump                                  |
| Game  | `Enter`  | Accept target position                |
| REPL  | arrows   | Move cursor/fill from command history |
| REPL  | `CTRL-C` | Interrupt running code immediately    |

## The `Spell` monad

`Spell` is the game's equivalent of `IO`. It currently exposes the following
primitives to the player:

```haskell
-- Shoots a firebolt in the given direction, or the last direction of movement
-- if passed a near-zero vector.
firebolt :: (Double, Double) -> Spell ()

-- Shows a target selector on the screen that can be controlled using arrow
-- keys. Blocks the REPL until the target is accepted using Enter,
inputTarget :: Spell (Double, Double)

-- Throws an exception in the Spell monad.
throwSpell :: Exception e => e -> Spell ()

-- Catches an exception in the Spell monad.
catch :: forall e a. Exception e => Spell a -> (e -> Spell a) -> Spell a

-- Sends a character to the REPL.
putChar :: Char -> Spell ()

-- Gets a character from the REPL.
getChar :: Spell Char
```

To encourage writing pure code, only a limited number of side effects can be
executed in a short period of time. Casting most side effects depletes the
player's side effect bar, and running out of side effects throws an exception to
the player's code.

## Current features

- Player movement.
- Basic REPL that can interpret arbitrary `Spell ()` expressions at runtime.
- A regenerating side effect bar that is depleted by casting spells. Throws an
  `OutOfSideEffects` exception to the player's code upon running out.

## Planned features

- Binding variables in the REPL.
- Player code sandboxing. (already implemented for printing uncaught precise exceptions)
- Code editor.
- Concurrency. (e.g. `forkSpell`)
- Enemies, items and levels.
- `Spell` side effects to probe the current game state, such as enemy and item positions.
