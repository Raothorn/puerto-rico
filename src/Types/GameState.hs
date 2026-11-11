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
  , plantationStack
  , players
  , playerL
  , faceUpPlantations
  -- , plantationL
  , numPlayersL
  , totalHolesL
  )
where

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
  = NewGamePhase
  | RoleActionPhase Role [Index Player]
  | ChooseRolePhase (S.Set Role) [Index Player]
  | ColonistShipResupplyPhase
  | NoPhase
  deriving (Eq, Show)

data GameState = GameState
  { _phaseStack :: [GamePhase]
  , _players :: [Player]
  , _faceUpPlantations :: [Plantation]
  , _plantationStack :: [Plantation]
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
newGame :: Int -> GameState
newGame numPlayers =
  GameState
    { _phaseStack = [ChooseRolePhase roles (map Index [0 .. numPlayers - 1])]
    , _players = map player [0 .. numPlayers - 1]
    , _faceUpPlantations = []
    , _plantationStack = Rules.initPlantationStack numPlayers
    , _availableBuildings = Rules.initialAvailableBuildings
    , _quarryStack = 8
    , _colonistShip = 0
    , _colonistSupply = 0
    , _eventLog = []
    }
 where
  player pIx =
    newPlayer
      & doubloons .~ (numPlayers - 1)
      & tiles .~ [newTile (PlantationTile (Rules.startingPlantation numPlayers pIx))]
  roles = S.fromList [Builder, Mayor, Settler]

makeLenses ''GameState

-- | Focus on the player at a zero-based index.
playerL :: Index Player -> Traversal' GameState Player
playerL (Index playerIx) = players . ix playerIx

-- | Read the number of players in the game.
numPlayersL :: SimpleGetter GameState Int
numPlayersL = to $ length . _players

totalHolesL :: SimpleGetter GameState Int
totalHolesL = to $ sum . fmap (^. numHolesL) . _players
