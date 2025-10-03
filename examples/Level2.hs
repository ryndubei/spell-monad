-- # User input

-- TEXT: The drawbridge is suspended by 3 ropes. Make it drop down so you can
-- cross.
-- (Using inputTarget would be the easiest way to get the positions of each
-- rope as vectors)


-- would have to call this 3 times to proceed:
main :: Spell ()
main = do
  -- prompts interactively selecting a specific coordinate
  -- the screen (suppose a crosshair controlled by arrow keys),
  -- blocks further execution until the player has selected something
  p <- inputTarget :: Spell V2
  p0 <- playerPosition :: Spell V2
  -- overrides the default cast direction (wherever the player is currently
  -- facing) with the given vector until the next call to 'face'.
  -- 0 would reset it back to the default.
  face (p ^-^ p0) :: V2 -> Spell ()
  firebolt
