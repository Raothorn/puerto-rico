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
  | BoardTilesFull
  | NoColonistRemainingInSupply
  | NoColonistRemainingOnShip
  | InvalidPlantationIndex
  | TileAlreadyHasColonist
  | EmptyPhaseStack
  | NoError
