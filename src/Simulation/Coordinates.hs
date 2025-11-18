{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Coordinates (V, module Linear.V2) where

import FRP.Yampa
import Linear.V2
import qualified Linear as L

-- | Continuous 2D coordinate type
type V = L.V2 Double

instance (Eq a, Floating a) => VectorSpace (L.V2 a) a where
  zeroVector = L.zero
  (*^) = (L.*^)
  (^/) = (L.^/)
  (^+^) = (L.^+^)
  (^-^) = (L.^-^)
  negateVector = L.negated
  dot = L.dot
  norm = L.norm
  normalize = L.signorm
