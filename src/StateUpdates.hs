module StateUpdates
  ( takeQuarry
  , takePlantation
  , resupplyColonistShip
  , placeTile
  , placeColonist
  , popPhase
  , pushPhase
  )
where

import Control.Monad
import Lens.Micro
import Lens.Micro.Mtl

import Rules
import Types
import Util

endRound :: Update GameState ()
endRound = do
  -- Phase stack should be empty
  numPlayers <- use numPlayersL
  currentMayor %= mapIndex (mod numPlayers . (+ 1))

resupplyColonistShip :: Update GameState ()
resupplyColonistShip = do
  desiredColonists <- use totalHolesL
  return ()

takeColonistFromShip :: Update GameState ()
takeColonistFromShip = do
  remainingColonists <- colonistShip <%= (subtract 1)
  when (remainingColonists < 0) $ liftErr NoColonistRemainingOnShip

-- | Remove a quarry from the stack, failing when the stack is empty.
takeQuarry :: Update GameState TileType
takeQuarry = do
  remainingQuarries <- quarryStack <%= (`subtract` 1)
  when (remainingQuarries < 0) $ liftErr NoQuarryTilesRemaining
  return QuarryTile

-- | Take a face-up plantation and leave its slot empty.
takePlantation :: Index Plantation -> Update GameState TileType
takePlantation plantationIx = do
  (plantation, plantations') <-
    useMaybe InvalidPlantationIndex (faceUpPlantations . to (removeAt plantationIx))

  faceUpPlantations .= plantations'
  return (PlantationTile plantation)

placeColonist :: ColonistPlacement -> Update Player ()
placeColonist placement = do
  case placement of
    BuildingPlacement _ -> return ()
    TilePlacement tileIx -> do
      zoom (tileL tileIx) $ do
        alreadyHasColonist <- use tileHasColonist
        when alreadyHasColonist $ liftErr TileAlreadyHasColonist
        tileHasColonist .= True

-- | Place a tile on a player's board if there is an available slot.
placeTile :: TileType -> Update Player ()
placeTile tType = do
  let tile = newTile tType
  numTiles <- use $ tiles . to length
  when (numTiles >= boardTileSlots) $ liftErr BoardTilesFull
  tiles %= (tile :)

placeBuilding :: BuildingType -> Update Player ()
placeBuilding bType = do
  numBuildings <- use $ buildings . to length
  when (numBuildings >= 12) $ liftErr BuildingSlotsFull
  buildings %= (newBuilding bType :)

-- | Remove and return the current phase, failing when the stack is empty.
popPhase :: Update GameState GamePhase
popPhase = do
  curPhaseStack <- use phaseStack
  case curPhaseStack of
    [] -> liftErr EmptyPhaseStack
    curPhase : remainingPhases -> do
      phaseStack .= remainingPhases
      return curPhase

-- | Make a phase the current phase by pushing it onto the stack.
pushPhase :: GamePhase -> Update GameState ()
pushPhase phase = phaseStack %= (phase :)
