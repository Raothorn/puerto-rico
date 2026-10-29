module Types.Action
  ( Action (..)
  , RoleAction (..)
  , SettlerAction (..)
  , MayorAction (..)
  , ColonistPlacement (..)
  )
where

import Types.Player
import Types.Shared

data Action
  = RoleAction RoleAction
  | ChooseRoleAction Role

data RoleAction
  = SettlerAction SettlerAction
  | MayorAction MayorAction
  | OptOut

data SettlerAction
  = TakeQuarryAction
  | TakePlantationAction (Index Plantation)

-- | The flag requests the mayor's privilege; the list is the full new staffing layout.
data MayorAction = PlaceColonistsAction Bool [ColonistPlacement]

data ColonistPlacement
  = TileColonist (Index Tile)
  | BuildingColonist (Index Building)
