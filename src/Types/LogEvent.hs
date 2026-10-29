module Types.LogEvent
  ( LogEvent (..)
  )
where

import Types.Player
import Types.Shared

data LogEvent
  = PlantationPlaced (Index Player) Plantation
  | NoEvent
