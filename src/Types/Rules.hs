{-# LANGUAGE TemplateHaskell #-}

module Types.Rules
  ( GameRules
  , getGameRules
  , numTileSlots
  )
where

import Lens.Micro.TH (makeLenses)

data GameRules = GameRules
  { _numTileSlots :: Int
  , _numFaceUpPlantations :: Int
  }

makeLenses ''GameRules

-- | Construct the rules for a game with the given number of players.
getGameRules :: Int -> GameRules
getGameRules numPlayers =
  GameRules
    { _numTileSlots = 12
    , _numFaceUpPlantations = numPlayers + 1
    }
