{-# LANGUAGE TemplateHaskell #-}

module Types.Rules
  ( GameRules
  , boardTileSlots
  , getGameRules
  , numFaceUpPlantations
  , initPlayerPlantations
  , initPlantationStack
  )
where

import Data.List ((\\))
import Lens.Micro.TH

import Types.Tile

data GameRules = GameRules
  { _numFaceUpPlantations :: Int
  , _initPlayerPlantations :: [Plantation]
  , _initPlantationStack :: [Plantation]
  }

makeLenses ''GameRules

boardTileSlots :: Int
boardTileSlots = 12

-- | Construct the rules for a game with the given number of players.
getGameRules :: Int -> GameRules
getGameRules numPlayers =
  GameRules
    { _numFaceUpPlantations = numPlayers + 1
    , _initPlayerPlantations = startingPlantations
    , _initPlantationStack = startingPlantationStack \\ startingPlantations
    }
 where
  startingPlantations = case numPlayers of
    3 -> [Indigo, Indigo, Corn]
    4 -> [Indigo, Indigo, Corn, Corn]
    5 -> [Indigo, Indigo, Indigo, Corn, Corn]
    _ -> []

  startingPlantationStack =
    (replicate 8 Coffee)
      ++ (replicate 9 Tobacco)
      ++ (replicate 10 Corn)
      ++ (replicate 11 Sugar)
      ++ (replicate 12 Indigo)
