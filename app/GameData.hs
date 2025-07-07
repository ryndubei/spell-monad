module GameData (GameData, Level(..)) where
import Simulation

-- | level data, saves, scripts, etc
data GameData

newtype Level = Level { initialSimState :: SimState }
