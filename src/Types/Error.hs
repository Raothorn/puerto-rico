module Types.Error
  ( Error (..)
  )
where

data Error
  = NoEmptyPlantationTiles
  | NoRemainingPlayers
  | NoPlayerFound
  | NoQuarryTilesRemaining
  | IndexNotFound
  | NoPrivilege
  | WrongPhase
  | WrongPlayer
  | BoardTilesFull
  | NoColonistRemainingInSupply
  | NoColonistRemainingOnShip
  | InsufficientDoubloons
  | InvalidPlantationIndex
  | TileAlreadyHasColonist
  | EmptyPhaseStack
  | BuildingSlotsFull
  | NoError
  deriving (Eq, Show)
