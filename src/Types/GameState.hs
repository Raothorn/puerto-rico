{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.GameState
  ( GameState
  , GamePhase (..)
  , newGame
  -- Lenses
  , eventLog
  , colonistSupply
  , colonistShip
  , quarryStack
  , phaseStack
  , players
  , playerL
  , plantationL
  , numPlayersL
  , rulesL
  , totalHolesL
  )
where

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

import Types.LogEvent
import Types.Player
import Types.Rules
import Types.Shared

data GamePhase
  = NewGamePhase
  | RoleActionPhase Role [Index Player]
  | ChooseRolePhase (S.Set Role) [Index Player]
  | ColonistShipResupplyPhase
  | NoPhase

data GameState = GameState
  { _phaseStack :: [GamePhase]
  , _players :: M.Map (Index Player) Player
  , _faceUpPlantations :: M.Map (Index Plantation) (Maybe Plantation)
  , _plantationStack :: [Plantation]
  , _plantationDiscard :: [Plantation]
  , _quarryStack :: Int
  , _colonistShip :: Int
  , _colonistSupply :: Int
  , _eventLog :: [LogEvent]
  }

newGame :: Int -> GameState
newGame numPlayers =
  GameState
    { _phaseStack = [NewGamePhase]
    , _players = playerListToMap (replicate numPlayers newPlayer)
    , _faceUpPlantations = M.empty
    , _plantationStack = []
    , _plantationDiscard = []
    , _quarryStack = 8
    , _colonistShip = 0
    , _colonistSupply = 0
    , _eventLog = []
    }
 where
  playerListToMap =
    M.fromList
      . F.toList
      . Seq.mapWithIndex (\pIx p -> (Index (pIx + 1), p))
      . Seq.fromList

makeLenses ''GameState

-- | Derive the game rules from the number of players.
rulesL :: SimpleGetter GameState GameRules
rulesL = numPlayersL . to getGameRules

-- | Focus on the player at an index
playerL :: Index Player -> Traversal' GameState Player
playerL playerIx = players . ix playerIx

-- | Focus on a face-up plantation slot
plantationL :: Index Plantation -> Traversal' GameState (Maybe Plantation)
plantationL plantationIx = faceUpPlantations . ix plantationIx

-- | Read the number of players in the game.
numPlayersL :: SimpleGetter GameState Int
numPlayersL = to $ length . _players

totalHolesL :: SimpleGetter GameState Int
totalHolesL = to $ sum . fmap (^. numHolesL) . _players
