module Types
  ( module Types.Action
  , module Types.GameState
  , module Types.LogEvent
  , module Types.Player
  , module Types.Rules
  , module Types.Building
  , module Types.Tile
  , module Types.Index
  , module Types.Role
  , Error (..)
  , Update
  )
where

import Control.Monad.Trans.State.Lazy

import Types.Action
import Types.Building
import Types.GameState
import Types.Index
import Types.LogEvent
import Types.Player
import Types.Role
import Types.Rules
import Types.Tile

data Error
  = NoEmptyPlantationTiles
  | NoRemainingPlayers
  | NoPlayerFound
  | NoQuarryTilesRemaining
  | IndexNotFound
  | NoPrivilege
  | WrongPhase
  | WrongPlayer (Index Player) (Index Player)
  | BoardTilesFull
  | NoColonistRemainingInSupply
  | NoColonistRemainingOnShip
  | InvalidPlantationIndex
  | TileAlreadyHasColonist
  | EmptyPhaseStack
  | BuildingSlotsFull
  | NoError
  deriving (Eq, Show)

type Update s a = StateT s (Either Error) a
