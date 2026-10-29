module Types.Shared
  ( Plantation (..)
  , Index (..)
  , Update
  , Role (..)
  , Tile (..)
  )
where

import Control.Monad.Trans.State.Lazy

import Types.Error

data Plantation = Tobacco | Sugar | Coffee
  deriving (Eq, Ord)

data Tile
  = PlantationTile Plantation
  | QuarryTile

data Role = Settler | Mayor | Builder | Craftsman | Trader

newtype Index a = Index Int
  deriving (Eq, Ord)

type Update s a = StateT s (Either Error) a
