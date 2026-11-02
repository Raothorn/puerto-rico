{-# LANGUAGE TemplateHaskell #-}

module Types.Building
  ( Building
  , BuildingType (..)
  , buildingType
  , hasColonist
  , newBuilding
  ) where

import Lens.Micro.TH

data Building = Building
  { _buildingType :: BuildingType
  , _hasColonist :: Bool
  }

data BuildingType
  = SmallIndigoPlant
  | SmallMarket

makeLenses ''Building

newBuilding :: BuildingType -> Building
newBuilding bType = Building bType False
