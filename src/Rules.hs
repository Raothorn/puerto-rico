module Rules
  ( boardTileSlots
  , numFaceUpPlantations
  , startingPlantation
  , initPlantationStack
  , initialAvailableBuildings
  , isBuildingLarge
  , buildingCost
  , buildingHoles
  )
where

import Data.List (lookup, (!?), (\\))
import Data.Maybe

import Types.Building
import Types.Tile

boardTileSlots :: Int
boardTileSlots = 12

-- | Number of plantations to reveal for the given number of players.
numFaceUpPlantations :: Int -> Int
numFaceUpPlantations numPlayers = numPlayers + 1

startingPlantation :: Int -> Int -> Maybe Plantation
startingPlantation numPlayers playerIx = do
  -- (initPlayerPlantations numPlayers) !? playerIx
  initPs <- initPlayerPlantations numPlayers
  initPs !? playerIx

-- | Plantation supply after removing the players' starting plantations.
initPlantationStack :: Int -> Maybe [Plantation]
initPlantationStack numPlayers = do
  initPs <- initPlayerPlantations numPlayers
  return $ startingPlantationStack \\ initPs
 where
  startingPlantationStack =
    (replicate 8 Coffee)
      ++ (replicate 9 Tobacco)
      ++ (replicate 10 Corn)
      ++ (replicate 11 Sugar)
      ++ (replicate 12 Indigo)

initPlayerPlantations :: Int -> Maybe [Plantation]
initPlayerPlantations 3 = Just [Indigo, Indigo, Corn]
initPlayerPlantations 4 = Just [Indigo, Indigo, Corn, Corn]
initPlayerPlantations 5 = Just [Indigo, Indigo, Indigo, Corn, Corn]
initPlayerPlantations _ = Just []

initialAvailableBuildings :: [(BuildingType, Int)]
initialAvailableBuildings =
  map (\(t, _, _) -> (t, if isBuildingLarge t then 1 else 2)) buildingInfo

isBuildingLarge :: BuildingType -> Bool
isBuildingLarge GuildHall = True
isBuildingLarge Residence = True
isBuildingLarge Fortress = True
isBuildingLarge CustomsHouse = True
isBuildingLarge CityHall = True
isBuildingLarge _ = False

buildingHoles :: BuildingType -> Maybe Int
buildingHoles bType = lookup bType (map (\(t, _, h) -> (t, h)) buildingInfo)

buildingCost :: BuildingType -> Maybe Int
buildingCost bType = lookup bType (map (\(t, c, _) -> (t, c)) buildingInfo)

-- | (Type, Cost, Holes)
buildingInfo :: [(BuildingType, Int, Int)]
buildingInfo =
  [ (SmallIndigoPlant, 1, 1)
  , (LargeIndigoPlant, 3, 3)
  , (SmallSugarMill, 2, 1)
  , (LargeSugarMill, 4, 3)
  , (TobaccoStorage, 5, 3)
  , (CoffeeRoaster, 6, 2)
  , (SmallMarket, 1, 1)
  , (LargeMarket, 5, 1)
  , (Hacienda, 2, 1)
  , (ConstructionHut, 2, 1)
  , (SmallWarehouse, 3, 1)
  , (LargeWarehouse, 6, 1)
  , (Hospice, 4, 1)
  , (Office, 5, 1)
  , (Factory, 7, 1)
  , (University, 8, 1)
  , (Harbor, 8, 1)
  , (Wharf, 9, 1)
  , (GuildHall, 10, 1)
  , (Residence, 10, 1)
  , (Fortress, 10, 1)
  , (CustomsHouse, 10, 1)
  , (CityHall, 10, 1)
  ]
