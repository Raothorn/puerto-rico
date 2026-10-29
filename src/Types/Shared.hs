module Types.Shared
  ( Plantation (..)
  , Index
  , Error (..)
  , Update
  , Role (..)
  , Action (..)
  , RoleAction (..)
  , SettlerAction (..)
  , Tile (..)
  )
where

import Control.Monad.Trans.State.Lazy

data Plantation = Tobacco | Sugar | Coffee
  deriving (Eq, Ord)

data Tile
  = PlantationTile Plantation
  | QuarryTile

data Role = Settler | Mayor | Builder | Craftsman | Trader

data Action
  = RoleAction RoleAction
  | ChooseRoleAction Role

data RoleAction
  = SettlerAction SettlerAction
  | MayorAction
  | OptOut

data SettlerAction
  = TakeQuarryAction
  | TakePlantationAction (Index Plantation)

newtype Index a = Index Int
  deriving (Eq, Ord)

data Error
  = NoEmptyPlantationTiles
  | NoRemainingPlayers
  | NoPlayerFound
  | StackEmpty
  | IndexNotFound
  | NoPrivilege
  | WrongPhase
  | BoardTilesFull
  | InvalidPlantationIndex
  | EmptyPhaseStack
  | NoError

type Update s a = StateT s (Either Error) a
