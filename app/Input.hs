module Input (Angle, fromRadians, toRadians, UserInput(..)) where

import Data.Text (Text)
import Data.Fixed (mod')

newtype Angle = Angle { toRadians :: Double }
  deriving (Eq, Show)

fromRadians :: Double -> Angle
fromRadians r = Angle (r `mod'` (2*pi)) 

data UserInput
  = ReplLine Text
  | Move Angle
  | Pause
  | Unpause
  | Point Angle Double
