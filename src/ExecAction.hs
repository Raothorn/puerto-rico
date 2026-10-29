module ExecAction
  ( execAction
  , execRoleAction
  )
where

import Control.Monad
import Lens.Micro
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

  unless (roleMatchesAction role roleAction) $ liftErr WrongRole

  numPlayers <- use numPlayersL
  let hasPrivilege = length remainingPlayerIxs == numPlayers
  (curPlayerIx, remainingPlayerIxs') <- case remainingPlayerIxs of
    [] -> liftErr NoRemainingPlayers
    playerIx : playerIxs -> pure (playerIx, playerIxs)

  _ <- preuse (playerL curPlayerIx) >>= liftMaybe NoPlayerFound
  case role of
    Mayor -> do
      when hasPrivilege $ distributeColonists remainingPlayerIxs
      case roleAction of
        OptOut -> zoom (playerL curPlayerIx) keepColonists
        _ -> execRoleAction curPlayerIx hasPrivilege roleAction
      when (null remainingPlayerIxs') refillColonistShip
    _ -> execRoleAction curPlayerIx hasPrivilege roleAction

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

execRoleAction playerIx hasPrivilege (MayorAction (PlaceColonistsAction takePrivilege placements)) = do
  _ <- preuse (playerL playerIx) >>= liftMaybe NoPlayerFound
  when (takePrivilege && not hasPrivilege) $ liftErr NoPrivilege
  numTaken <- if takePrivilege then takeColonist else pure 0
  zoom (playerL playerIx) $ do
    colonists += numTaken
    placeColonists placements

execRoleAction _ _ OptOut = logEvent NoEvent

roleMatchesAction :: Role -> RoleAction -> Bool
roleMatchesAction Settler (SettlerAction _) = True
roleMatchesAction Mayor (MayorAction _) = True
roleMatchesAction _ OptOut = True
roleMatchesAction _ _ = False
