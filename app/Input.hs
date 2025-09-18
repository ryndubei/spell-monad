module Input (UserInput(..), moveVector) where

import Control.Lens

-- | Aggregated input of the user over a short period of time.
data UserInput = UserInput
  { moveX :: !Double
  , moveY :: !Double
  } deriving Show

moveVector :: Lens' UserInput (Double, Double)
moveVector = lens (\UserInput{moveX, moveY} -> (moveX, moveY)) (\u (x,y) -> u {moveX = x, moveY = y})

-- | Commutative up to floating-point errors.
instance Semigroup UserInput where
  (<>) u1 u2 = UserInput
    { moveX = moveX u1 + moveX u2
    , moveY = moveY u1 + moveY u2
    }

instance Monoid UserInput where
  mempty = UserInput
    { moveX = 0
    , moveY = 0
    }
