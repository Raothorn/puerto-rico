{-# LANGUAGE TemplateHaskell #-}

module Types.Rules
  ( GameRules
  , getGameRules
  , numTileSlots
  )
where

import Lens.Micro.TH

data GameRules = GameRules
  { _numTileSlots :: Int
  , _numFaceUpPlantations :: Int
  }

makeLenses ''GameRules

getGameRules :: Int -> GameRules
getGameRules numPlayers =
  GameRules
    { _numTileSlots = 12
    , _numFaceUpPlantations = numPlayers + 1
    }
