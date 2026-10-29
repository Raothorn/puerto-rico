module ExecAction
  ( execRoleAction
  )
where

import Control.Monad (unless)
import Lens.Micro.Mtl (use, zoom)

import StateUpdates
import Types
import Util

execAction :: Action -> Update GameState ()
execAction (ChooseRoleAction _) = do
  -- TODO: Implement role selection.
  pure ()
execAction (RoleAction roleAction) = do
  currentPhase <- popPhase
  (role, remainingPlayerIndices) <- case currentPhase of
    RoleActionPhase currentRole playerIndices -> pure (currentRole, playerIndices)
    _ -> liftErr WrongPhase

  numPlayers <- use numPlayersL
  let hasPrivilege = length remainingPlayerIndices == numPlayers
  (playerIndex, remainingPlayerIndices') <- case remainingPlayerIndices of
    [] -> liftErr NoRemainingPlayers
    nextPlayerIndex : otherPlayerIndices -> pure (nextPlayerIndex, otherPlayerIndices)

  execRoleAction playerIndex hasPrivilege roleAction

  if null remainingPlayerIndices'
    -- The role phase has been popped, exposing the next phase on the stack.
    then pure ()
    else pushPhase $ RoleActionPhase role remainingPlayerIndices'

-- | Execute a player's role action with the supplied privilege status.
execRoleAction :: Index Player -> Bool -> RoleAction -> Update GameState ()
execRoleAction playerIndex hasPrivilege (SettlerAction settlerAction) = do
  tile <- case settlerAction of
    TakeQuarryAction -> do
      unless hasPrivilege $ liftErr NoPrivilege
      takeQuarry
    TakePlantationAction plantationIndex -> takePlantation plantationIndex
  rules <- use rulesL
  zoom (playerL playerIndex) $ placeTile rules tile

execRoleAction _ _ MayorAction = do
  -- TODO: Implement the mayor action.
  pure ()

execRoleAction _ _ OptOut = logEvent NoEvent

getRoleFromAction :: RoleAction -> Role
getRoleFromAction (SettlerAction _) = Settler
getRoleFromAction _ = Mayor
