module Rules
  ( boardTileSlots
  , numFaceUpPlantations
  , initPlayerPlantations
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

-- | Starting plantations in player order for the given number of players.
initPlayerPlantations :: Int -> [Plantation]
initPlayerPlantations numPlayers = case numPlayers of
  3 -> [Indigo, Indigo, Corn]
  4 -> [Indigo, Indigo, Corn, Corn]
  5 -> [Indigo, Indigo, Indigo, Corn, Corn]
  _ -> []

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
