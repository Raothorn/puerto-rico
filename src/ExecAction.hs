module ExecAction
  ( execRoleAction
  )
where

import Control.Monad
import Lens.Micro
import Lens.Micro.Mtl

import StateUpdates
import Types
import Util

execAction :: Action -> Update GameState ()
execAction (ChooseRoleAction role) = do
  -- TODO
  return ()
execAction (RoleAction action) = do
  curPhase <- popPhase
  -- Check Valid Phase
  (role, remainingPlayers) <- case curPhase of
    RoleActionPhase r rp -> return (r, rp)
    _ -> liftErr WrongPhase

  numPlayers <- use numPlayersL
  let hasPrivilege = length remainingPlayers == numPlayers
  (curPlayer, remainingPlayers') <- case remainingPlayers of
    [] -> liftErr NoRemainingPlayers
    x : xs -> return (x, xs)

  execRoleAction curPlayer hasPrivilege action

  if null remainingPlayers'
    then return () -- Phase is already popped, ChooseRoleAction should be the current phase
    else pushPhase $ RoleActionPhase role remainingPlayers'

execRoleAction :: (Index Player) -> Bool -> RoleAction -> Update GameState ()
--------------------
-- SETTLER ACTION --
--------------------
execRoleAction curPlayer hasPrivilege (SettlerAction action) = do
  tile <- case action of
    TakeQuarryAction -> do
      when (not hasPrivilege) $ liftErr NoPrivilege
      takeQuarry
    TakePlantationAction plantationIx -> takePlantation plantationIx
  rules <- use rulesL
  zoom (playerL curPlayer) $ placeTile rules tile

------------------
-- Mayor Action --
------------------
execRoleAction pIx _ MayorAction = do
  -- TODO
  return ()

-----------
-- TITLE --
-----------
execRoleAction pIx _ _ = logEvent NoEvent

getRoleFromAction :: RoleAction -> Role
getRoleFromAction (SettlerAction _) = Settler
getRoleFromAction _ = Mayor
