module Types.Error
  ( Error (..)
  )
where

import Types.GameState
import Types.Index
import Types.Player
import Types.Role

data Error
  = NoEmptyPlantationTiles
  | NoRemainingPlayers
  | NoPlayerFound
  | NoQuarryTilesRemaining
  | IndexNotFound
  | InvalidPlayerCount
  | NoPrivilege
  | WrongPhase GamePhase GamePhase
  | WrongPlayer (Index Player) (Index Player)
  | BoardTilesFull
  | NoColonistRemainingInSupply
  | NoColonistRemainingOnShip
  | InsufficientDoubloons
  | InvalidPlantationIndex
  | TileAlreadyHasColonist
  | EmptyPhaseStack
  | BuildingSlotsFull
  | RoleNotAvailable Role
  | OnlyMayorCanEndRound
  | NoError
  deriving (Eq, Show)
