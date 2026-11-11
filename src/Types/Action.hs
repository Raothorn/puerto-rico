module Types.Action
  ( Action (..)
  , RoleAction (..)
  , SettlerAction (..)
  , ColonistSource (..)
  , ColonistPlacement (..)
  )
where

import Types.Building
import Types.Index
import Types.Role
import Types.Tile

data Action
  = RoleAction RoleAction
  | ChooseRoleAction Role
  | ResupplyColonistShip
  | EndRoundAction

data RoleAction
  = SettlerAction SettlerAction
  | MayorAction ColonistSource ColonistPlacement
  | BuilderAction BuildingType

data SettlerAction
  = TakeQuarryAction
  | TakePlantationAction (Index Plantation)

data ColonistSource = Ship | Supply

data ColonistPlacement
  = BuildingPlacement (Index Building)
  | TilePlacement (Index Tile)
