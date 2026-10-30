{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Player
  ( Player
  , buildings
  , tiles
  , numHolesL
  , buildingL
  , tileL
  )
where

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Shared

data Player = Player
  { _tiles :: [Tile]
  , _buildings :: [Building]
  , _doubloons :: Int
  }

makeLenses ''Player

numHolesL :: SimpleGetter Player Int
numHolesL = to numHoles
 where
  numHoles = length . filter (not . (^. hasColonist)) . _tiles

buildingL :: Index Building -> Traversal' Player Building
buildingL (Index buildingIx) = buildings . ix buildingIx

tileL :: Index Tile -> Traversal' Player Tile
tileL (Index tileIx) = tiles . ix tileIx
