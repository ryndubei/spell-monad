module Input (Angle(..), UserInput(..)) where

import Data.Text (Text)

data Relative = Relative { relX :: Rational, relY :: Rational }

newtype Angle = Angle Double -- ^ Radians.

data UserInput = ReplLine Text | Move Angle | Pause 
