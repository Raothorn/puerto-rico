{-# LANGUAGE TemplateHaskell #-}

module Types.Tile
  ( Tile
  , TileType (..)
  , tileType
  , hasColonist
  , newTile
  ) where

import Lens.Micro.TH
import Types.Shared

data Tile = Tile
  { _tileType :: TileType
  , _hasColonist :: Bool
  }

data TileType
  = PlantationTile Plantation
  | QuarryTile

makeLenses ''Tile

newTile :: TileType -> Tile
newTile tType = Tile tType False
