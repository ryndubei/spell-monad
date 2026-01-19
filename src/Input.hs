module Input (UserInput(..), userInputMoveVector, nullInput) where

import Control.Lens
import Simulation.Coordinates
import Linear.Epsilon

-- | Aggregated input of the user over a short period of time.
data UserInput = UserInput
  { moveX :: !Double
  , moveY :: !Double
  , jump :: !Bool
  , enter :: !Bool
  } deriving Show

userInputMoveVector :: Lens' UserInput V
userInputMoveVector = lens (\UserInput{moveX, moveY} -> V2 moveX moveY) (\u (V2 x y) -> u {moveX = x, moveY = y})

-- | Commutative up to floating-point errors.
instance Semigroup UserInput where
  (<>) u1 u2 = UserInput
    { moveX = moveX u1 + moveX u2
    , moveY = moveY u1 + moveY u2
    , jump = jump u1 || jump u2
    , enter = enter u1 || enter u2
    }

instance Monoid UserInput where
  mempty = UserInput
    { moveX = 0
    , moveY = 0
    , jump = False
    , enter = False
    }

-- | Whether the user input can be ignored.
--
-- >>> nullInput mempty
-- True
nullInput :: UserInput -> Bool
nullInput UserInput{moveX, moveY, jump, enter}
  | jump = False
  | enter = False
  | nearZero (V2 moveX moveY) = True
  | otherwise = False
