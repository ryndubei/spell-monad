module Input (UserInput(..), moveVector) where

import Control.Lens
import qualified Linear as L

-- | Aggregated input of the user over a short period of time.
data UserInput = UserInput
  { togglePause :: !Bool
  , moveX :: !Double
  , moveY :: !Double
  , jump :: !Bool
  } deriving Show

moveVector :: Lens' UserInput (L.V2 Double)
moveVector = lens (\UserInput{moveX, moveY} -> L.V2 moveX moveY) (\u (L.V2 x y) -> u {moveX = x, moveY = y})

-- | Commutative up to floating-point errors.
instance Semigroup UserInput where
  (<>) u1 u2 = UserInput
    -- We could be clever and have e.g. togglePause
    -- discard any inputs made while it is on, but that
    -- loses us commutativity for relatively little benefit
    -- (with a high simulation frequency, it would be difficult
    -- to distinguish between space + ESC and ESC + space)
    { togglePause = togglePause u1 || togglePause u2 -- could also use xor
    , moveX = moveX u1 + moveX u2
    , moveY = moveY u1 + moveY u2
    , jump = jump u1 || jump u2
    }

instance Monoid UserInput where
  mempty = UserInput
    { togglePause = False
    , moveX = 0
    , moveY = 0
    , jump = False
    }
