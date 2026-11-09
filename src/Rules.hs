module Rules
  ( boardTileSlots
  , numFaceUpPlantations
  , startingPlantation
  , initPlantationStack
  )
where

import Data.List ((\\))

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
