{-# LANGUAGE TemplateHaskell #-}

module Types.Tile
  ( Tile
  , TileType (..)
  , tileType
  , hasColonist
  ) where

import Lens.Micro.TH

data Tile = Tile
  { _tileType :: TileType
  , _hasColonist :: Bool
  }

data TileType
  = SmallIndigoPlant
  | SmallMarket

makeLenses ''Tile

newTile :: TileType -> Tile
newTile tType = Tile tType False
