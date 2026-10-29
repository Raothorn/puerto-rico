module Types.Error
  ( Error (..)
  )
where

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
  | WrongRole
  | InvalidTileIndex
  | InvalidBuildingIndex
  | DuplicateTilePlacement
  | ColonistSlotsFull
  | InvalidColonistCount
  | InvalidPlayerOrder
