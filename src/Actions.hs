module Actions
  ( execRoleAction
  )
where

import Lens.Micro.Mtl

import StateUpdates
import Types

execRoleAction :: (Index Player) -> RoleAction -> Update GameState ()
execRoleAction pIx roleAction = do
  curPhase <- use phase
  nextPhase <- case curPhase of
    RoleActionPhase usedRoles finishedPlayers -> do
      execRoleAction' pIx roleAction
      totalPlayers <- use numPlayersL
      if length finishedPlayers >= totalPlayers
        then return NoPhase
        else
          return $
            RoleActionPhase
              (getRoleFromAction roleAction : usedRoles)
              (pIx : finishedPlayers)
    _ -> liftErr WrongPhase

  phase .= nextPhase

execRoleAction' :: (Index Player) -> RoleAction -> Update GameState ()
--------------------
-- SETTLER ACTION --
--------------------
execRoleAction' pIx (SettlerAction plantation) = do
  placePlantation pIx plantation
  logEvent $ PlantationPlaced pIx plantation

------------------
-- Mayor Action --
------------------
execRoleAction' pIx MayorAction = do
  return ()

-----------
-- TITLE --
-----------
execRoleAction' pIx _ = logEvent NoEvent

getRoleFromAction :: RoleAction -> Role
getRoleFromAction (SettlerAction _) = Settler
getRoleFromAction _ = Mayor
