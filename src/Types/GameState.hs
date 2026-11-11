{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.GameState
  ( GameState
  , GamePhase (..)
  , RoundPhase (..)
  , RolePhase (..)
  , newGame
  -- Lenses
  , eventLog
  , colonistSupply
  , colonistShip
  , currentMayor
  , quarryStack
  , gamePhase
  , plantationStack
  , gamePlayers
  , playerL
  , faceUpPlantations
  -- , plantationL
  , numPlayersL
  , totalHolesL
  )
where

import Data.Maybe
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH

import qualified Data.Map as M
import qualified Data.Set as S

import Types.Building
import Types.Index
import Types.LogEvent
import Types.Player
import Types.Role
import Types.Tile

import qualified Rules

data GamePhase
  = InRound [Role] [Index Player] RoundPhase
  | GameOver
  deriving (Eq, Show)

data RoundPhase
  = ChoosingRole
  | ResolvingRole RolePhase [Index Player]
  deriving (Eq, Show)

data RolePhase
  = ResolvingSettler
  | ResolvingBuilder
  deriving (Eq, Show)

-- data SettlerPhase = SettlerPhase

data GameState = GameState
  { _gamePhase :: GamePhase
  , _gamePlayers :: [Player]
  , _faceUpPlantations :: [Plantation]
  , _plantationStack :: [Plantation]
  , _currentMayor :: Index Player
  , -- , _plantationDiscard :: [Plantation]
    _quarryStack :: Int
  , _availableBuildings :: M.Map BuildingType Int
  , _colonistShip :: Int
  , _colonistSupply :: Int
  , _eventLog :: [LogEvent]
  }

-----------------
-- Constructor --
-----------------

-- | Construct a game with each player's starting doubloons and plantation.
newGame :: Int -> Maybe GameState
newGame numPlayers = do
  plantationStack <- Rules.initPlantationStack numPlayers
  let roles = [Builder, Mayor, Settler]
  return $
    GameState
      { _gamePhase = InRound roles (map Index [0 .. numPlayers - 1]) $ ChoosingRole
      , _gamePlayers = mapMaybe (newPlayer numPlayers) [0 .. numPlayers - 1]
      , _faceUpPlantations = []
      , _plantationStack = plantationStack
      , _currentMayor = Index 0
      , _availableBuildings = M.fromList Rules.initialAvailableBuildings
      , _quarryStack = 8
      , _colonistShip = 0
      , _colonistSupply = 0
      , _eventLog = []
      }

makeLenses ''GameState

-- | Focus on the player at a zero-based index.
playerL :: Index Player -> Traversal' GameState Player
playerL (Index playerIx) = gamePlayers . ix playerIx

-- | Read the number of players in the game.
numPlayersL :: SimpleGetter GameState Int
numPlayersL = to $ length . _gamePlayers

totalHolesL :: SimpleGetter GameState Int
totalHolesL = to $ sum . fmap (^. numHolesL) . _gamePlayers
