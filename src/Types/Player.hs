{-# LANGUAGE TemplateHaskell #-}

module Types.Player
  ( Player
  , tiles
  , buildings
  , doubloons
  )
where

import Lens.Micro.TH (makeLenses)

import Types.Shared

data Player = Player
  { _tiles :: [Tile]
  , _buildings :: [Index Building]
  , _doubloons :: Int
  }

data Building

makeLenses ''Player
