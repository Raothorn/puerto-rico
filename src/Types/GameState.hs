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

import Lens.Micro (SimpleGetter, Traversal', ix, to)
import Lens.Micro.GHC ()
import Lens.Micro.TH (makeLenses)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Types.LogEvent
import Types.Player
import Types.Rules
import Types.Shared

data GamePhase
  = RoleActionPhase Role [Index Player]
  | ChooseRolePhase (Set.Set Role) [Index Player]
  | NoPhase

data GameState = GameState
  { _phaseStack :: [GamePhase]
  , _players :: Map.Map (Index Player) Player
  , _faceUpPlantations :: Map.Map (Index Plantation) (Maybe Plantation)
  , _plantationDiscard :: [Plantation]
  , _quarryStack :: Int
  , _colonistShip :: Int
  , _colonistStack :: Int
  , _eventLog :: [LogEvent]
  }

makeLenses ''GameState

-- | Derive the game rules from the number of players.
rulesL :: SimpleGetter GameState GameRules
rulesL = numPlayersL . to getGameRules

-- | Focus on the player at an index, with no target if the index is absent.
playerL :: Index Player -> Traversal' GameState Player
playerL playerIndex = players . ix playerIndex

-- | Focus on a face-up plantation slot, with no target if the index is absent.
plantationL :: Index Plantation -> Traversal' GameState (Maybe Plantation)
plantationL plantationIndex = faceUpPlantations . ix plantationIndex

-- | Read the number of players in the game.
numPlayersL :: SimpleGetter GameState Int
numPlayersL = players . to length
