module StateUpdates
  ( takeQuarry
  , takePlantation
  , placeTile
  , popPhase
  , pushPhase
  )
where

import Control.Monad
import Data.Map
import Lens.Micro
import Lens.Micro.Mtl

import Types
import Util

initGame :: Update GameState ()
initGame = do
  numPlayers <- use numPlayersL
  -- Give each player doubloons
  players . traversed . doubloons .= numPlayers - 1

  return ()

resupplyColonistShip :: Update GameState ()
resupplyColonistShip = do
  desiredColonists <- use totalHolesL
  return ()

takeColonistFromShip :: Update GameState ()
takeColonistFromShip = do
  remainingColonists <- colonistShip <%= (`subtract` 1)
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
  plantation <-
    preuse (plantationL plantationIx)
      >>= liftMaybe InvalidPlantationIndex -- invalid traversal
      >>= liftMaybe InvalidPlantationIndex -- no plantation in chosen slot
  plantationL plantationIx .= Nothing
  return (PlantationTile plantation)

-- | Place a tile on a player's board if there is an available slot.
placeTile :: GameRules -> TileType -> Update Player ()
placeTile rules tileType = do
  let tile = newTile tileType
  let maxTiles = rules ^. numTileSlots
  numTiles <- use $ tiles . to length
  when (numTiles >= maxTiles) $ liftErr BoardTilesFull
  tiles %= (tile :)

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
