module ExecAction
  ( execRoleAction
  )
where

import Control.Monad
import Lens.Micro.Mtl

import StateUpdates
import Types
import Util

execAction :: Action -> Update GameState ()
execAction (ChooseRoleAction _) = do
  -- TODO: Implement role selection.
  pure ()
execAction (RoleAction roleAction) = do
  curPhase <- popPhase
  (role, remainingPlayerIxs) <- case curPhase of
    RoleActionPhase curRole playerIxs -> pure (curRole, playerIxs)
    _ -> liftErr WrongPhase

  numPlayers <- use numPlayersL
  let hasPrivilege = length remainingPlayerIxs == numPlayers
  (curPlayerIx, remainingPlayerIxs') <- case remainingPlayerIxs of
    [] -> liftErr NoRemainingPlayers
    playerIx : playerIxs -> pure (playerIx, playerIxs)

  execRoleAction curPlayerIx hasPrivilege roleAction

  if null remainingPlayerIxs'
    -- The role phase has been popped, exposing the next phase on the stack.
    then pure ()
    else pushPhase $ RoleActionPhase role remainingPlayerIxs'

-- | Execute a player's role action with the supplied privilege status.
execRoleAction :: Index Player -> Bool -> RoleAction -> Update GameState ()
execRoleAction playerIx hasPrivilege (SettlerAction settlerAction) = do
  tile <- case settlerAction of
    TakeQuarryAction -> do
      unless hasPrivilege $ liftErr NoPrivilege
      takeQuarry
    TakePlantationAction plantationIx -> takePlantation plantationIx
  rules <- use rulesL
  zoom (playerL playerIx) $ placeTile rules tile

execRoleAction _ _ MayorAction = do
  -- TODO: Implement the mayor action.
  pure ()

execRoleAction _ _ OptOut = logEvent NoEvent

getRoleFromAction :: RoleAction -> Role
getRoleFromAction (SettlerAction _) = Settler
getRoleFromAction _ = Mayor
