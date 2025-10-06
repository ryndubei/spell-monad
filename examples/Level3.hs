-- # Search

-- TEXT: The door is locked. The key is hidden somewhere here, but you don't
-- have to look for it yourself.

-- (the level should be very large)

data SpaceTree = SpaceTree
  { here :: [Item] -- could be expanded to more than just items, e.g.
                   -- have information about walls, enemies, whatever
  , coordinate :: V2
  , up :: SpaceTree 
  , left :: SpaceTree
  , right :: SpaceTree
  , down :: SpaceTree
  }


main :: Spell ()
main = do
  -- Returns the current state of the entire level as an infinite, lazy tree.
  -- The argument is the radius (in the game's distance unit) that each node
  -- of the tree represents.
  --
  -- The tree is heavily coupled to the game's state, hence it would be impossible
  -- to have this function in a non-Haskell implementation of the game that relies
  -- on some separate process to handle compilation.
  spaceTree <- (scan :: Double -> Spell SpaceTree) 1
  let keyApproxCoordinate = someTreeSearchAlgorithmImplementedByPlayer spaceTree
  print keyApproxCoordinate -- and then the player navigates there and retrieves the key
