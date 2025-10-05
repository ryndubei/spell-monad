{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
module Simulation (SimState(..), SimEvent(..), simSF, ObjectIdentifier(..), VisibleObject(..)) where

import FRP.Yampa
import Input
import Data.Text (Text)
import qualified Data.Text as T
import Simulation.Input
import Control.Lens
import Data.Sequence (Seq)

-- | Tells the UI thread how an object should be drawn.
data ObjectIdentifier = Player deriving (Eq, Ord, Show)

data VisibleObject = VisibleObject
  { position :: !(Double, Double) -- ^ centre
  , radius :: Double -- ^ anything further away from 'position' should not be drawn
  , sdf :: (Double, Double) -> Double -- ^ signed distance function to be drawn, relative to position
  , objIdentifier :: ObjectIdentifier
  }

data SimState = SimState
  { objects :: [VisibleObject]
  , camera :: !(Double, Double) -- ^ the 'camera' position should always be visible, even
                          -- if not necessarily in the centre of the screen
  }

data SimEvent = SimEvent
  { simLogs :: Seq Text
  , gameOver :: !Bool
  }

instance Semigroup SimEvent where
  (<>) s1 s2 = SimEvent
    { simLogs = simLogs s1 <> simLogs s2
    , gameOver = gameOver s1 || gameOver s2
    }

instance Monoid SimEvent where
  mempty = SimEvent { simLogs = mempty, gameOver = False }

simSF :: SF (Event UserInput) (SimState, Event SimEvent)
simSF = proc u -> do
  vInput <- arr (over moveVector (10 *^)) <<< processInput -< u

  pos <- trapezoidIntegral -< vInput ^. moveVector

  returnA -< (SimState
    { objects = [playerObject pos]
    , camera = (0, 0)
    }, fmap evt u)
  where
    evt u = SimEvent { gameOver = False, simLogs = pure . T.pack $ show u}
    playerObject pos = VisibleObject
        { position = pos
        , radius = 3
        -- square of diameter 2
        , sdf = \(x,y) -> max (abs x - 1) (abs y - 1)
        , objIdentifier = Player
        }
