module Types.Action
  ( Action (..)
  , RoleAction (..)
  , SettlerAction (..)
  )
where

import Types.Shared (Index, Plantation, Role)

data Action
  = RoleAction RoleAction
  | ChooseRoleAction Role

data RoleAction
  = SettlerAction SettlerAction
  | MayorAction
  | OptOut

data SettlerAction
  = TakeQuarryAction
  | TakePlantationAction (Index Plantation)
