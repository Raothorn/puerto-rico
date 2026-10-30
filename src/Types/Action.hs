module Types.Action
  ( Action (..)
  , RoleAction (..)
  , SettlerAction (..)
  , ColonistSource (..)
  , ColonistPlacement (..)
  )
where

import Types.Shared

data Action
  = RoleAction RoleAction
  | ChooseRoleAction Role
  | ResupplyColonistShip

data RoleAction
  = SettlerAction SettlerAction
  | MayorAction ColonistSource ColonistPlacement
  | OptOut

data SettlerAction
  = TakeQuarryAction
  | TakePlantationAction (Index Plantation)

data ColonistSource = Ship | Supply

data ColonistPlacement
  = BuildingPlacement (Index Building)
  | TilePlacement (Index Tile)
