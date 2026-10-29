module StateUpdates
  ( takeQuarry
  , takePlantation
  , placeTile
  , popPhase
  , pushPhase
  )
where

import Control.Monad
import Lens.Micro
import Lens.Micro.Mtl

import Types
import Util

takeQuarry :: Update GameState Tile
takeQuarry = do
  quarries <- use quarryStack
  when (quarries <= 0) $
    liftErr StackEmpty

  quarryStack -= 1

  return QuarryTile

takePlantation :: Index Plantation -> Update GameState Tile
takePlantation plantationIx = do
  chosenPlantation <-
    preuse (plantationL plantationIx)
      >>= (liftMaybe InvalidPlantationIndex) -- invalid traversal
      >>= (liftMaybe InvalidPlantationIndex) -- no plantation in chosen slot
  plantationL plantationIx .= Nothing
  return (PlantationTile chosenPlantation)

placeTile :: GameRules -> Tile -> Update Player ()
placeTile rules tile = do
  let maxTiles = rules ^. numTileSlots
  numTiles <- use $ tiles . to length
  when (numTiles >= maxTiles) $ liftErr BoardTilesFull
  tiles %= (tile :)

popPhase :: Update GameState GamePhase
popPhase = do
  curPhaseStack <- use phaseStack
  case curPhaseStack of
    [] -> liftErr EmptyPhaseStack
    p : ps -> do
      phaseStack .= ps
      return p

pushPhase :: GamePhase -> Update GameState ()
pushPhase phase = phaseStack %= (phase :)
