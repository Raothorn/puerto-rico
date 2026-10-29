{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Player
  ( Player
  , Building (..)
  , tiles
  , buildings
  , doubloons
  , colonists
  , tileColonists
  , buildingColonists
  , numColonistSlots
  , unassignedColonistsL
  , numEmptyBuildingSlotsL
  )
where

import Lens.Micro
import Lens.Micro.TH

import qualified Data.Map as M
import qualified Data.Set as S

import Types.Shared

data Player = Player
  { _tiles :: M.Map (Index Tile) Tile
  , _buildings :: M.Map (Index Building) Building
  , _doubloons :: Int
  , _colonists :: Int -- Total owned, including staffed colonists and those in San Juan.
  , _tileColonists :: S.Set (Index Tile)
  , _buildingColonists :: M.Map (Index Building) Int
  }

data Building = Building
  { _numColonistSlots :: Int
  }

makeLenses ''Player
makeLenses ''Building

-- | Read the colonists in San Juan rather than on a tile or building.
unassignedColonistsL :: SimpleGetter Player Int
unassignedColonistsL = to $ \player ->
  player ^. colonists
    - S.size (player ^. tileColonists)
    - sum (player ^. buildingColonists)

-- | Count vacant building slots, excluding plantations and quarries.
numEmptyBuildingSlotsL :: SimpleGetter Player Int
numEmptyBuildingSlotsL = to $ \player ->
  sum
    [ building ^. numColonistSlots - M.findWithDefault 0 buildingIx (player ^. buildingColonists)
    | (buildingIx, building) <- M.toList (player ^. buildings)
    ]
