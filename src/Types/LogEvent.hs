module Types.LogEvent
  ( LogEvent (..)
  )
where

import Types.Index
import Types.Player
import Types.Tile

data LogEvent
  = PlantationPlaced (Index Player) Plantation
  | NoEvent
