{-# LANGUAGE LinearTypes #-}

-- # Resource management

-- TEXT: The path is blocked by a wall, but none of your spells right now are
-- strong enough to break it down on their own. Use the conveniently-placed
-- boulder at the wall's weakpoint to destroy the wall. 
--
-- Physical objects are handled using _linear types_. In a function of type
-- `a %1 -> b`, `a` cannot be freely duplicated and discarded.
-- If you only have one `Rock %1`, you will not be able to call
-- Rock %1 -> Rock %1 -> Spell ().
--
-- But currently, we just have one object that we care about, so this does not mean
-- much. The only important thing is that linear functions which do not consume
-- the object (like `getObjectPosition`) will return it.


-- (NOTE: linear types are still somewhat half-baked in GHC, may end up not being
-- able to use them. They will also 


data PhysicalObjectTag = LargeBoulder -- | ...

data PhysicalObject = PhysicalObject
  {
  -- ...
  }

unbindObject :: PhysicalObject %1 -> Spell ()

-- the object does not get consumed, so we can call this repeatedly
fireObject :: V2 -> PhysicalObject %1 -> Spell (PhysicalObject %1)

-- Nothing if no object was matched
withPhysicalObject :: (PhysicalObjectTag -> Bool) -> (PhysicalObject %1 -> Spell a) -> Spell (Maybe a)

main :: IO ()
main = do
  _ <- withPhysicalObject (== LargeBoulder) $ \b -> do
    spaceTree <- scan -- see Level3
    let weakpoint = someTreeSearchAlgorithmImplementedByPlayer spaceTree
    (pos, b') <- getObjectPosition b
    b'' <- fireObject (weakpoint ^-^ pos) b'
    -- To get rid of (o :: PhysicalObject %1),
    -- we can also just do
    -- ```
    -- catch ((throw DummyException) o) (\DummyException -> pure ())
    -- ```
    -- Therefore we cannot assume that unbindObject will always be called.
    unbindObject b''

