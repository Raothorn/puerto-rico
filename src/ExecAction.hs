module ExecAction
  ( execRoleAction
  )
where

import Control.Monad
import Lens.Micro.Mtl

import StateUpdates
import Types
import Util

execAction :: Index Player -> Action -> Update GameState ()
--------------------
-- Role Selection --
--------------------
execAction playerIx (ChooseRoleAction role) = do
  -- TODO: Implement role selection.
  curPhase <- popPhase
  (remainingRoles, remainingPlayers) <- case curPhase of
    ChooseRolePhase roles players -> return (roles, players)
    _ -> liftErr WrongPhase

  return ()
execAction playerIx ResupplyColonistShip = do
  curPhase <- popPhase
  case curPhase of
    ColonistShipResupplyPhase -> resupplyColonistShip
    _ -> liftErr WrongPhase

------------------
-- Role actions --
------------------
execAction playerIx (RoleAction roleAction) = do
  curPhase <- popPhase
  (role, remainingPlayerIxs) <- case curPhase of
    RoleActionPhase curRole playerIxs -> return (curRole, playerIxs)
    _ -> liftErr WrongPhase

  numPlayers <- use numPlayersL
  let hasPrivilege = length remainingPlayerIxs == numPlayers
  (curPlayerIx, remainingPlayerIxs') <- case remainingPlayerIxs of
    [] -> liftErr NoRemainingPlayers
    playerIx : playerIxs -> return (playerIx, playerIxs)

  unless (playerIx == curPlayerIx) $ liftErr (WrongPlayer curPlayerIx playerIx)
  execRoleAction curPlayerIx hasPrivilege roleAction

  if null remainingPlayerIxs'
    -- The role phase has been popped, exposing the next phase on the stack.
    then return ()
    else pushPhase $ RoleActionPhase role remainingPlayerIxs'
execAction _ _ = return ()

--------------------
-- Settler Action --
--------------------

-- | Execute a player's role action with the supplied privilege status.
execRoleAction :: Index Player -> Bool -> RoleAction -> Update GameState ()
execRoleAction playerIx hasPrivilege (SettlerAction settlerAction) = do
  tile <- case settlerAction of
    TakeQuarryAction -> do
      unless hasPrivilege $ liftErr NoPrivilege
      takeQuarry
    TakePlantationAction plantationIx -> takePlantation plantationIx
  rules <- use rulesL
  zoom (playerL playerIx) $ placeTile tile

------------------
-- Mayor Action --
------------------
execRoleAction playerIx hasPrivilege (MayorAction source placement) = do
  -- Ignore the privileged action for now
  remainingColonists <- colonistShip <%= (`subtract` 1)

  zoom (playerL playerIx) $ do
    placeColonist placement

  when (remainingColonists <= 0) $
    pushPhase ColonistShipResupplyPhase
execRoleAction _ _ _ = logEvent NoEvent

--------------------
-- Builder Action --
--------------------
-- execRoleAction playerIx hasPrivilege (Building)

getRoleFromAction :: RoleAction -> Role
getRoleFromAction (SettlerAction _) = Settler
getRoleFromAction _ = Mayor
