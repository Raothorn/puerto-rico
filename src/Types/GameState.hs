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
  , faceUpPlantations
  -- , plantationL
  , numPlayersL
  , rulesL
  , totalHolesL
  )
where

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH

import qualified Data.Foldable
import qualified Data.Map as M
import qualified Data.Sequence
import qualified Data.Set as S

import Types.Index
import Types.LogEvent
import Types.Player
import Types.Role
import Types.Rules as Rules
import Types.Tile

data GamePhase
  = NewGamePhase
  | RoleActionPhase Role [Index Player]
  | ChooseRolePhase (S.Set Role) [Index Player]
  | ColonistShipResupplyPhase
  | NoPhase
  deriving (Eq, Show)

data GameState = GameState
  { _phaseStack :: [GamePhase]
  , _players :: M.Map (Index Player) Player
  , _faceUpPlantations :: [Plantation]
  , _plantationStack :: [Plantation]
  , _plantationDiscard :: [Plantation]
  , _quarryStack :: Int
  , _colonistShip :: Int
  , _colonistSupply :: Int
  , _eventLog :: [LogEvent]
  }

-----------------
-- Constructor --
-----------------
newGame :: Int -> GameState
newGame numPlayers =
  GameState
    { _phaseStack = [ChooseRolePhase roles (M.keys players)]
    , _players = players
    , _faceUpPlantations = []
    , _plantationStack = rules ^. initPlantationStack
    , _plantationDiscard = []
    , _quarryStack = 8
    , _colonistShip = 0
    , _colonistSupply = 0
    , _eventLog = []
    }
 where
  playerListToMap =
    M.fromList
      . Data.Foldable.toList
      . Data.Sequence.mapWithIndex (\pIx p -> (Index (pIx + 1), p))
      . Data.Sequence.fromList
  players = playerListToMap (replicate numPlayers newPlayer)

  rules = getGameRules numPlayers
  roles = S.fromList [Builder, Mayor, Settler]

makeLenses ''GameState

-- | Derive the game rules from the number of players.
rulesL :: SimpleGetter GameState GameRules
rulesL = numPlayersL . to getGameRules

-- | Focus on the player at an index
playerL :: Index Player -> Traversal' GameState Player
playerL playerIx = players . ix playerIx

-- | Read the number of players in the game.
numPlayersL :: SimpleGetter GameState Int
numPlayersL = to $ length . _players

totalHolesL :: SimpleGetter GameState Int
totalHolesL = to $ sum . fmap (^. numHolesL) . _players
