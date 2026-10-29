{-# LANGUAGE TemplateHaskell #-}

module Types.Player
  ( Player
  , tiles
  , buildings
  , dubloons
  )
where

import Lens.Micro.TH

import Types.Shared

data Player = Player
  { _tiles :: [Tile]
  , _buildings :: [Index Building]
  , _dubloons :: Int
  }

data Building

makeLenses ''Player
