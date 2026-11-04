{-# LANGUAGE TemplateHaskell #-}

module Types.Tile
  ( Tile
  , TileType (..)
  , Plantation (..)
  , tileType
  , tileHasColonist
  , newTile
  ) where

import Lens.Micro.TH

data Plantation = Coffee | Sugar | Indigo | Tobacco | Corn
  deriving (Eq, Show)

data Tile = Tile
  { _tileType :: TileType
  , _tileHasColonist :: Bool
  }
  deriving (Eq, Show)

data TileType
  = PlantationTile Plantation
  | QuarryTile
  deriving (Eq, Show)

makeLenses ''Tile

newTile :: TileType -> Tile
newTile tType = Tile tType False
