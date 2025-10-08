module Input (UserInput(..), userInputMoveVector) where

import Control.Lens

-- | Aggregated input of the user over a short period of time.
data UserInput = UserInput
  { moveX :: !Double
  , moveY :: !Double
  , jump :: !Bool
  } deriving Show

userInputMoveVector :: Lens' UserInput (Double, Double)
userInputMoveVector = lens (\UserInput{moveX, moveY} -> (moveX, moveY)) (\u (x,y) -> u {moveX = x, moveY = y})

-- | Commutative up to floating-point errors.
instance Semigroup UserInput where
  (<>) u1 u2 = UserInput
    { moveX = moveX u1 + moveX u2
    , moveY = moveY u1 + moveY u2
    , jump = jump u1 || jump u2
    }

instance Monoid UserInput where
  mempty = UserInput
    { moveX = 0
    , moveY = 0
    , jump = False
    }
