{-# LANGUAGE TemplateHaskell #-}
module Types.Player
  ( Player
  , PlayerBoard
  , board
  , plantations
  , buildings
  , dubloons
  )
where

import Lens.Micro.TH
import Types.Shared

data Player = Player
  { _board :: PlayerBoard
  }

data Building

data PlayerBoard = PlayerBoard
  { _plantations :: [Plantation],
    _buildings :: [Index Building],
    _dubloons :: Int
  }

makeLenses ''Player
makeLenses ''PlayerBoard
