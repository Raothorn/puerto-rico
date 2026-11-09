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
  , totalHolesL
  )
where

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH

import qualified Data.Set as S

import Rules
import Types.Index
import Types.LogEvent
import Types.Player
import Types.Role
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
  , _players :: [Player]
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
-- | Construct a game with each player's starting doubloons and plantation.
newGame :: Int -> GameState
newGame numPlayers =
  GameState
    { _phaseStack = [ChooseRolePhase roles (map Index [0 .. numPlayers - 1])]
    , _players = players
    , _faceUpPlantations = []
    , _plantationStack = initPlantationStack numPlayers
    , _plantationDiscard = []
    , _quarryStack = 8
    , _colonistShip = 0
    , _colonistSupply = 0
    , _eventLog = []
    }
 where
  players =
    take numPlayers $
      map startingPlayer (startingTiles ++ repeat [])
  startingTiles =
    map (\plantation -> [newTile (PlantationTile plantation)]) (initPlayerPlantations numPlayers)
  startingPlayer initialTiles =
    newPlayer
      & doubloons .~ (numPlayers - 1)
      & tiles .~ initialTiles

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
