module Types.Shared
  ( Plantation (..)
  , Index
  , Error (..)
  , Update
  , Role(..)
  , RoleAction(..)
  )
where

import Control.Monad.Trans.State.Lazy

data Plantation = Tobacco | Sugar | Coffee | Quarry
  deriving (Eq, Ord)

data Role = Settler | Mayor | Builder | Craftsman | Trader

data RoleAction
  = SettlerAction Plantation
  | MayorAction

newtype Index a = Index Int
  deriving (Eq, Ord)

data Error
  = NoEmptyPlantationTiles
  | NoPlantationsLeftInStack
  | NoPlayerFound
  | IndexNotFound
  | WrongPhase
  | NoError

type Update s a = StateT s (Either Error) a
