{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.GameState
  ( GameState
  , GamePhase (..)
  -- Lenses
  , eventLog
  , colonistStack
  , quarryStack
  , phaseStack
  , playerL
  , plantationL
  , numPlayersL
  , rulesL
  )
where

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl
import Lens.Micro.TH

import qualified Data.Map as M
import qualified Data.Set as S

import Types.LogEvent
import Types.Player
import Types.Rules
import Types.Shared

data GamePhase
  = RoleActionPhase Role [Index Player]
  | ChooseRolePhase (S.Set Role) [Index Player]
  | NoPhase

data GameState = GameState
  { _phaseStack :: [GamePhase]
  , _players :: M.Map (Index Player) Player
  , _faceUpPlantations :: M.Map (Index Plantation) (Maybe Plantation)
  , -- , -- , _plantationStacks :: M.Map Plantation Int
    _plantationDiscard :: [Plantation]
  , _quarryStack :: Int
  , _colonistShip :: Int
  , _colonistStack :: Int
  , _eventLog :: [LogEvent]
  }

makeLenses ''GameState

rulesL :: SimpleGetter GameState GameRules
rulesL = numPlayersL . to getGameRules

playerL :: (Index Player) -> Traversal' GameState Player
playerL pIx = players . ix pIx

plantationL :: Index Plantation -> Traversal' GameState (Maybe Plantation)
plantationL plantationIx = faceUpPlantations . ix plantationIx

numPlayersL :: SimpleGetter GameState Int
numPlayersL = players . to length
