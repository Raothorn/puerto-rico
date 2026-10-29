module StateUpdates
  ( takeQuarry
  , takePlantation
  , placeTile
  , popPhase
  , pushPhase
  )
where

import Control.Monad (when)
import Lens.Micro (to, (^.))
import Lens.Micro.Mtl (preuse, use, (%=), (-=), (.=))

import Types
import Util

-- | Remove a quarry from the stack, failing when the stack is empty.
takeQuarry :: Update GameState Tile
takeQuarry = do
  numQuarries <- use quarryStack
  when (numQuarries <= 0) $
    liftErr StackEmpty

  quarryStack -= 1

  pure QuarryTile

-- | Take a face-up plantation and leave its slot empty.
takePlantation :: Index Plantation -> Update GameState Tile
takePlantation plantationIndex = do
  plantation <-
    preuse (plantationL plantationIndex)
      >>= liftMaybe InvalidPlantationIndex -- invalid traversal
      >>= liftMaybe InvalidPlantationIndex -- no plantation in chosen slot
  plantationL plantationIndex .= Nothing
  pure (PlantationTile plantation)

-- | Place a tile on a player's board if there is an available slot.
placeTile :: GameRules -> Tile -> Update Player ()
placeTile rules tile = do
  let maxTiles = rules ^. numTileSlots
  numTiles <- use $ tiles . to length
  when (numTiles >= maxTiles) $ liftErr BoardTilesFull
  tiles %= (tile :)

-- | Remove and return the current phase, failing when the stack is empty.
popPhase :: Update GameState GamePhase
popPhase = do
  currentPhaseStack <- use phaseStack
  case currentPhaseStack of
    [] -> liftErr EmptyPhaseStack
    currentPhase : remainingPhases -> do
      phaseStack .= remainingPhases
      pure currentPhase

-- | Make a phase the current phase by pushing it onto the stack.
pushPhase :: GamePhase -> Update GameState ()
pushPhase phase = phaseStack %= (phase :)
