{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Player
  ( Player
  , newPlayer
  , buildings
  , tiles
  , doubloons
  , numHolesL
  , buildingL
  , tileL
  )
where

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH
import Types.Building as B
import Types.Tile as T

import Types.Shared

data Player = Player
  { _tiles :: [Tile]
  , _buildings :: [Building]
  , _doubloons :: Int
  }

-- Value initialization happens in state
newPlayer :: Player
newPlayer =
  Player
    { _tiles = []
    , _buildings = []
    , _doubloons = 0
    }

makeLenses ''Player

numHolesL :: SimpleGetter Player Int
numHolesL = to numHoles
 where
  numHoles = length . filter (not . (^. B.hasColonist)) . _buildings

buildingL :: Index Building -> Traversal' Player Building
buildingL (Index buildingIx) = buildings . ix buildingIx

tileL :: Index Tile -> Traversal' Player Tile
tileL (Index tileIx) = tiles . ix tileIx
