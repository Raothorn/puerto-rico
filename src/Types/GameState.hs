{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.GameState
  ( GameState
  , GamePhase (..)
  -- Lenses
  , eventLog
  , colonistStack
  , colonistShip
  , players
  , lastRound
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
  , _plantationDiscard :: [Plantation]
  , _quarryStack :: Int
  , _colonistShip :: Int
  , _colonistStack :: Int
  , _eventLog :: [LogEvent]
  , _lastRound :: Bool
  }

makeLenses ''GameState

-- | Derive the game rules from the number of players.
rulesL :: SimpleGetter GameState GameRules
rulesL = numPlayersL . to getGameRules

-- | Focus on the player at an index, with no target if the index is absent.
playerL :: Index Player -> Traversal' GameState Player
playerL playerIx = players . ix playerIx

-- | Focus on a face-up plantation slot, with no target if the index is absent.
plantationL :: Index Plantation -> Traversal' GameState (Maybe Plantation)
plantationL plantationIx = faceUpPlantations . ix plantationIx

-- | Read the number of players in the game.
numPlayersL :: SimpleGetter GameState Int
numPlayersL = players . to length
