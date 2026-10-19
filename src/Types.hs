{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  ( Error(..),
    Update,
    Player,
    PlayerBoard,
    Plantation,
    GameState,
    Index,
    -- Lenses
    players,
    board,
    plantations,
    plantationStacks,
    playerL,
  )
where

import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.GHC
import Lens.Micro.TH

data Error
  = NoEmptyPlantationTiles
  | NoPlantationsLeftInStack
  | NoPlayerFound
  | NotFoundError
  | NoError

type Update s a = StateT s (Either Error) a

---------------
-- Gamestate --
---------------

data Player = Player 
    { _board :: PlayerBoard

    }

data Plantation = Tobacco | Sugar | Coffee
    deriving (Eq, Ord)

newtype Index a = Index Int
  deriving (Eq, Ord)

data Building

data PlayerBoard = PlayerBoard
  { _plantations :: [Plantation]
  , _buildings :: [Index Building]
  , _dubloons :: Int
  }

data GamePhase

data LogEvent

data GameState = GameState
  { _phase :: GamePhase
  , _players :: M.Map (Index Player) Player
  , _plantationStacks :: M.Map Plantation Int
  , _log :: [LogEvent]
  }

------------
-- Lenses --
------------
makeLenses ''Player
makeLenses ''PlayerBoard
makeLenses ''GameState

playerL :: (Index Player) -> Traversal' GameState (Player)
playerL pIx = players . ix pIx
--
-- plantationStackL 
