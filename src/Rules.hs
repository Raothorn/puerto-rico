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

import Data.List (lookup, (\\))

import qualified Data.Map as M

import Types.Building
import Types.Tile

boardTileSlots :: Int
boardTileSlots = 12

-- | Number of plantations to reveal for the given number of players.
numFaceUpPlantations :: Int -> Int
numFaceUpPlantations numPlayers = numPlayers + 1

startingPlantation :: Int -> Int -> Plantation
startingPlantation numPlayers playerIx = (initPlayerPlantations numPlayers) !! playerIx

-- | Plantation supply after removing the players' starting plantations.
initPlantationStack :: Int -> [Plantation]
initPlantationStack numPlayers = startingPlantationStack \\ initPlayerPlantations numPlayers
 where
  startingPlantationStack =
    (replicate 8 Coffee)
      ++ (replicate 9 Tobacco)
      ++ (replicate 10 Corn)
      ++ (replicate 11 Sugar)
      ++ (replicate 12 Indigo)

initPlayerPlantations :: Int -> [Plantation]
initPlayerPlantations 3 = [Indigo, Indigo, Corn]
initPlayerPlantations 4 = [Indigo, Indigo, Corn, Corn]
initPlayerPlantations 5 = [Indigo, Indigo, Indigo, Corn, Corn]
initPlayerPlantations _ = []

initialAvailableBuildings :: M.Map BuildingType Int
initialAvailableBuildings =
  M.fromList $
    map (\(t, _, _) -> (t, if isBuildingLarge t then 1 else 2)) buildingInfo

isBuildingLarge :: BuildingType -> Bool
isBuildingLarge GuildHall = True
isBuildingLarge Residence = True
isBuildingLarge Fortress = True
isBuildingLarge CustomsHouse = True
isBuildingLarge CityHall = True
isBuildingLarge _ = False

buildingHoles :: BuildingType -> Int
buildingHoles bType =
  case holes of
    Just h -> h
    Nothing -> 0
 where
  holes = lookup bType (map (\(t, _, h) -> (t, h)) buildingInfo)

buildingCost :: BuildingType -> Int
buildingCost bType =
  case cost of
    Just c -> c
    Nothing -> 0
 where
  cost = lookup bType (map (\(t, c, _) -> (t, c)) buildingInfo)

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
