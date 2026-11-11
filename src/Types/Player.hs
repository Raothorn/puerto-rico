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

import Data.Maybe
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH

import Types.Building
import Types.Index
import Types.Tile

import qualified Rules

data Player = Player
  { _tiles :: [Tile]
  , _buildings :: [Building]
  , _doubloons :: Int
  }
  deriving (Eq, Show)

newPlayer :: Int -> Int -> Maybe Player
newPlayer numPlayers playerIx = do
  startingPlantation <- Rules.startingPlantation numPlayers playerIx
  return $ Player [newTile $ PlantationTile startingPlantation] [] (numPlayers - 1)

makeLenses ''Player

numHolesL :: SimpleGetter Player Int
numHolesL = to (sum . map numHoles . _buildings)
 where
  numHoles building = (buildingHoles building) - (building ^. buildingColonists)
  buildingHoles building = fromMaybe 0 $ Rules.buildingHoles (building ^. buildingType)

buildingL :: Index Building -> Traversal' Player Building
buildingL (Index buildingIx) = buildings . ix buildingIx

tileL :: Index Tile -> Traversal' Player Tile
tileL (Index tileIx) = tiles . ix tileIx
