{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.GameState
  ( GameState (..),
    GamePhase (..),
    -- Lenses
    phase,
    eventLog,
    plantationStacks,
    playerL,
    numPlayersL,
  )
where

import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH
import Types.LogEvent
import Types.Player
import Types.Shared

data GamePhase
  = RoleActionPhase [Role] [Index Player]
  | NoPhase

data GameState = GameState
  { _phase :: GamePhase
  , _players :: M.Map (Index Player) Player
  , _plantationStacks :: M.Map Plantation Int
  , _colonistShip :: Int
  , _eventLog :: [LogEvent]
  }

makeLenses ''GameState

playerL :: (Index Player) -> Traversal' GameState Player
playerL pIx = players . ix pIx

numPlayersL :: SimpleGetter GameState Int
numPlayersL = players . to length
