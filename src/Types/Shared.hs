{-# LANGUAGE TemplateHaskell #-}

module Types.Shared
  ( Plantation (..)
  , Index (..)
  , Update
  , Role (..)
  , TileType (..)
  , Tile
  , newTile
  , Building
  , tileType
  , hasColonist
  )
where

import Control.Monad.Trans.State.Lazy
import Lens.Micro.TH

import Types.Error

data Building

data Plantation = Tobacco | Sugar | Coffee
  deriving (Eq, Ord)

data Tile = Tile
  { _tileType :: TileType
  , _hasColonist :: Bool
  }

newTile :: TileType -> Tile
newTile tileType = Tile tileType False

data TileType
  = PlantationTile Plantation
  | QuarryTile

data Role = Settler | Mayor | Builder | Craftsman | Trader

newtype Index a = Index Int
  deriving (Eq, Ord)

type Update s a = StateT s (Either Error) a

makeLenses ''Tile
