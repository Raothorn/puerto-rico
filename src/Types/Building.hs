{-# LANGUAGE TemplateHaskell #-}

module Types.Building
  ( Building
  , BuildingType (..)
  , buildingType
  , buildingHasColonist
  , newBuilding
  ) where

import Lens.Micro.TH

data Building = Building
  { _buildingType :: BuildingType
  , _buildingHasColonist :: Bool
  }
  deriving (Eq, Show)

data BuildingType
  = SmallIndigoPlant
  | SmallMarket
  deriving (Eq, Show)

makeLenses ''Building

newBuilding :: BuildingType -> Building
newBuilding bType = Building bType False
