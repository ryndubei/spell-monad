{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'Additive' instances for 'MSF's that produce vector spaces. This allows
-- you to use vector operators with 'MSF's that output vectors, for example,
-- you can write:
--
-- @
-- msf1 :: MSF Input (V2 Double) -- defined however you want
-- msf2 :: MSF Input (V2 Double) -- defined however you want
-- msf3 :: MSF Input (V2 Double)
-- msf3 = msf1 ^+^ msf2
-- @
--
-- instead of
--
-- @
-- msf3 = (msf1 &&& msf2) >>> arr (uncurry (^+^))
-- @
--
--
-- Instances are provided for the type class 'Additive'.
module Data.MonadicStreamFunction.Instances.Additive where

-- External imports
import Linear (Additive(zero))

-- Internal imports
import Control.Arrow.Util              (constantly)
import Data.MonadicStreamFunction.Core (MSF)

-- | Vector-space instance for 'MSF's.
instance Monad m => Additive (MSF m a) where zero = constantly 0
