module StateUpdates
  ( takeQuarry
  , takePlantation
  , placeTile
  , popPhase
  , pushPhase
  , distributeColonists
  , takeColonist
  , placeColonists
  , keepColonists
  , refillColonistShip
  )
where

import Control.Monad
import Lens.Micro
import Lens.Micro.Mtl

import qualified Data.Map as M
import qualified Data.Set as S

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
takePlantation plantationIx = do
  plantation <-
    preuse (plantationL plantationIx)
      >>= liftMaybe InvalidPlantationIndex -- invalid traversal
      >>= liftMaybe InvalidPlantationIndex -- no plantation in chosen slot
  plantationL plantationIx .= Nothing
  pure (PlantationTile plantation)

-- | Place a tile on a player's board if there is an available slot.
placeTile :: GameRules -> Tile -> Update Player ()
placeTile rules tile = do
  let maxTiles = rules ^. numTileSlots
  numTiles <- use $ tiles . to length
  when (numTiles >= maxTiles) $ liftErr BoardTilesFull
  curTiles <- use tiles
  let nextTileIx = case M.lookupMax curTiles of
        Nothing -> Index 0
        Just (Index tileIx, _) -> Index (tileIx + 1)
  tiles . at nextTileIx .= Just tile

-- | Remove and return the current phase, failing when the stack is empty.
popPhase :: Update GameState GamePhase
popPhase = do
  curPhaseStack <- use phaseStack
  case curPhaseStack of
    [] -> liftErr EmptyPhaseStack
    curPhase : remainingPhases -> do
      phaseStack .= remainingPhases
      pure curPhase

-- | Make a phase the current phase by pushing it onto the stack.
pushPhase :: GamePhase -> Update GameState ()
pushPhase phase = phaseStack %= (phase :)

-- | Deal the ship in the supplied turn order, with the remainder going to earlier players.
distributeColonists :: [Index Player] -> Update GameState ()
distributeColonists playerIxs = do
  numPlayers <- use numPlayersL
  when (null playerIxs) $ liftErr NoRemainingPlayers
  when (S.size (S.fromList playerIxs) /= numPlayers || length playerIxs /= numPlayers) $
    liftErr InvalidPlayerOrder

  numColonists <- use colonistShip
  let (numEach, numExtra) = numColonists `divMod` numPlayers
  forM_ (zip playerIxs [0 ..]) $ \(playerIx, turnIx) -> do
    _ <- preuse (playerL playerIx) >>= liftMaybe NoPlayerFound
    let numTaken = numEach + if turnIx < numExtra then 1 else 0
    zoom (playerL playerIx) $ colonists += numTaken
  colonistShip .= 0

-- | Take the mayor's extra colonist if any remain in the supply.
takeColonist :: Update GameState Int
takeColonist = do
  numColonists <- use colonistStack
  if numColonists <= 0
    then pure 0
    else do
      colonistStack -= 1
      pure 1

-- | Replace the staffing layout, keeping only surplus colonists in San Juan.
placeColonists :: [ColonistPlacement] -> Update Player ()
placeColonists placements = do
  curTiles <- use tiles
  curBuildings <- use buildings
  numColonists <- use colonists
  let tileIxs = [tileIx | TileColonist tileIx <- placements]
      staffedTiles = S.fromList tileIxs
      staffedBuildings = M.fromListWith (+) [(buildingIx, 1) | BuildingColonist buildingIx <- placements]
      numSlots = M.size curTiles + sum (map (^. numColonistSlots) (M.elems curBuildings))

  when (S.size staffedTiles /= length tileIxs) $ liftErr DuplicateTilePlacement
  forM_ tileIxs $ \tileIx ->
    unless (M.member tileIx curTiles) $ liftErr InvalidTileIndex
  forM_ (M.toList staffedBuildings) $ \(buildingIx, numPlaced) -> do
    building <- M.lookup buildingIx curBuildings & liftMaybe InvalidBuildingIndex
    when (numPlaced > building ^. numColonistSlots) $ liftErr ColonistSlotsFull
  when (length placements /= min numColonists numSlots) $ liftErr InvalidColonistCount

  tileColonists .= staffedTiles
  buildingColonists .= staffedBuildings

-- | Keep the existing layout only if no additional colonists need to be placed.
keepColonists :: Update Player ()
keepColonists = do
  tileIxs <- use tileColonists
  buildingCounts <- use buildingColonists
  let placements =
        map TileColonist (S.toList tileIxs)
          ++ concat
            [ replicate numPlaced (BuildingColonist buildingIx)
            | (buildingIx, numPlaced) <- M.toList buildingCounts
            ]
  placeColonists placements

-- | Refill from vacant building slots and mark the last round if the supply is short.
refillColonistShip :: Update GameState ()
refillColonistShip = do
  numPlayers <- use numPlayersL
  curPlayers <- use players
  numColonists <- use colonistStack
  let numWanted = max numPlayers (sum (map (^. numEmptyBuildingSlotsL) (M.elems curPlayers)))
      numTaken = min numColonists numWanted
  colonistShip .= numTaken
  colonistStack -= numTaken
  when (numTaken < numWanted) $ lastRound .= True
