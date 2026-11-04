module Types.Action
  ( Action (..)
  , RoleAction (..)
  , SettlerAction (..)
  , ColonistSource (..)
  , ColonistPlacement (..)
  )
where

import Types.Shared

import Types.Building
import Types.Tile

data Action
  = RoleAction RoleAction
  | ChooseRoleAction Role
  | ResupplyColonistShip

data RoleAction
  = SettlerAction SettlerAction
  | MayorAction ColonistSource ColonistPlacement
  | BuilderAction BuildingType
  | OptOut

data SettlerAction
  = TakeQuarryAction
  | TakePlantationAction (Index Plantation)

data ColonistSource = Ship | Supply

data ColonistPlacement
  = BuildingPlacement (Index Building)
  | TilePlacement (Index Tile)
