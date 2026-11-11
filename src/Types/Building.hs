{-# LANGUAGE TemplateHaskell #-}

module Types.Building
  ( Building
  , BuildingType (..)
  , buildingType
  , buildingColonists
  , newBuilding
  ) where

import Lens.Micro.TH

import Types.Tile

data Building = Building
  { _buildingType :: BuildingType
  , _buildingColonists :: Int
  }
  deriving (Eq, Show)

data BuildingType
  = SmallIndigoPlant
  | LargeIndigoPlant
  | SmallSugarMill
  | LargeSugarMill
  | TobaccoStorage
  | CoffeeRoaster
  | SmallMarket
  | LargeMarket
  | Hacienda
  | ConstructionHut
  | SmallWarehouse
  | LargeWarehouse
  | Hospice
  | Office
  | Factory
  | University
  | Harbor
  | Wharf
  | GuildHall
  | Residence
  | Fortress
  | CustomsHouse
  | CityHall
  deriving (Eq, Ord, Show)

makeLenses ''Building

newBuilding :: BuildingType -> Building
newBuilding bType = Building bType 0
