-- # Exceptions

-- TEXT: Enemies will attack you, and their attacks will do damage.
-- If you take too much damage, the game will end.
--
-- Also, when you take damage, this will throw an _asynchronous exception_
-- `DamageException`.
-- You likely have already observed exceptions from running out of mana, or
-- from dividing by 0. 
-- But these exceptions tend to get out of the way: they don't occur until
-- the moment you do something wrong, and you can generally tell exactly what
-- caused them.
-- Meanwhile, an asynchronous exception can be emitted by anything in Spell. 
--
-- Several functions will let you recover from exceptions. For this, 'catch'
-- is recommended.


-- | could also do an exception hierarchy similar to IOError
data DamageException = DamageException { cause :: DamageCause, damage :: Int }

data DamageCause
  -- 'atkr' is an existential type, and Attack is an extensible data family.
  -- The player should be able to find out what 'atkr' is at runtime by pattern
  -- matching on 'attack'.
  = forall atkr. Attack { attack :: Attack atkr, attackerPosition :: V2 }
  -- other possibilities:
  | HardCollision { with :: Material, normalVector :: V2, collisionVelocity :: V2 }
  | Suffocation { durationNoAir :: DTime }
  -- ...


main :: Spell ()
main =
  p <-
    -- if we are interrupted midway through targeting, 
    -- use the source of the interrupt as a target
    catch inputTarget (\e@DamageException{cause} ->
      case cause of
        Attack{attackerPosition} -> pure attackerPosition
        _ -> throwSpell e
    )
  p0 <- playerPosition
  face (p0 ^-^ p)
  firebolt
