module Types.LogEvent where

import Types.Player
import Types.Shared

data LogEvent 
    = PlantationPlaced (Index Player) Plantation
    | NoEvent
