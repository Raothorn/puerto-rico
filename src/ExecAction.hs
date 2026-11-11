module ExecAction
  ( execAction
  )
where

import Control.Monad
import Data.List (delete)
import Lens.Micro.Mtl

import StateUpdates
import Types
import Util

import qualified Rules

execAction :: Index Player -> Action -> Update GameState ()
--------------------
-- Role Selection --
--------------------
execAction playerIx EndRoundAction = do
  numPlayers <- use numPlayersL
  mayorIx <- use currentMayor
  unless (playerIx == mayorIx) $ liftErr OnlyMayorCanEndRound

  mayorIx' <- currentMayor <%= mapIndex (mod numPlayers . (+ 1))
  players <- rotatePlayers (unwrapIndex mayorIx')

  roles <- liftMaybe InvalidPlayerCount (Rules.gameRoles numPlayers)
  gamePhase .= InRound roles players ChoosingRole
execAction playerIx (ChooseRoleAction role) = do
  numPlayers <- use numPlayersL
  curPhase <- use gamePhase
  case curPhase of
    InRound remainingRoles (currentPlayerIx : remainingPlayerIxs) ChoosingRole -> do
      unless (elem role remainingRoles) $ liftErr (RoleNotAvailable role)
      unless (playerIx == currentPlayerIx) $
        liftErr (WrongPlayer currentPlayerIx playerIx)

      let remainingRoles' = delete role remainingRoles
      players <- rotatePlayers (unwrapIndex currentPlayerIx)
      gamePhase .= InRound remainingRoles' remainingPlayerIxs (ResolvingRole rolePhase players)
    _ -> liftErr $ WrongPhase (InRound [] [] ChoosingRole) curPhase

  return ()
 where
  rolePhase = case role of
    Settler -> ResolvingSettler
    -- TODO other roles
    _ -> ResolvingBuilder
execAction _ _ = return ()

-- ------------------
-- -- Role actions --
-- ------------------
-- execAction playerIx (RoleAction roleAction) = do
--   curPhase <- popPhase
--   (role, remainingPlayerIxs) <- case curPhase of
--     RoleActionPhase curRole playerIxs -> return (curRole, playerIxs)
--     _ -> liftErr WrongPhase
--
--   numPlayers <- use numPlayersL
--   let hasPrivilege = length remainingPlayerIxs == numPlayers
--   (curPlayerIx, remainingPlayerIxs') <- case remainingPlayerIxs of
--     [] -> liftErr NoRemainingPlayers
--     pIx : pIxs -> return (pIx, pIxs)
--
--   unless (playerIx == curPlayerIx) $ liftErr WrongPlayer
--   execRoleAction curPlayerIx hasPrivilege roleAction
--
--   if null remainingPlayerIxs'
--     -- The role phase has been popped, exposing the next phase on the stack.
--     then return ()
--     else pushPhase $ RoleActionPhase role remainingPlayerIxs'
-- execAction _ _ = return ()
--
-- --------------------
-- -- Settler Action --
-- --------------------
--
-- -- | Execute a player's role action with the supplied privilege status.
-- execRoleAction :: Index Player -> Bool -> RoleAction -> Update GameState ()
-- execRoleAction playerIx hasPrivilege (SettlerAction settlerAction) = do
--   tile <- case settlerAction of
--     TakeQuarryAction -> do
--       unless hasPrivilege $ liftErr NoPrivilege
--       takeQuarry
--     TakePlantationAction plantationIx -> takePlantation plantationIx
--   zoomPlayer playerIx $ placeTile tile
--
-- --------------------
-- -- Builder Action --
-- --------------------
-- execRoleAction playerix hasPrivilege (BuilderAction bType) = do
--   return ()
--
-- ------------------
-- -- Mayor Action --
-- ------------------
-- execRoleAction playerIx hasPrivilege (MayorAction source placement) = do
--   -- Ignore the privileged action for now
--   remainingColonists <- colonistShip <%= (`subtract` 1)
--
--   zoom (playerL playerIx) $ do
--     placeColonist placement
--
--   when (remainingColonists <= 0) $
--     pushPhase ColonistShipResupplyPhase
-- execRoleAction _ _ _ = logEvent NoEvent
--
-- --------------------
-- -- Builder Action --
-- --------------------
-- -- execRoleAction playerIx hasPrivilege (Building)
--
----------------------
-- -- Helpers --
----------------------
--
-- zoomPlayer :: Index Player -> Update Player () -> Update GameState ()
-- zoomPlayer playerIx = zoomRequired NoPlayerFound (playerL playerIx)

rotatePlayers :: Int -> Update GameState [Index Player]
rotatePlayers n = do
  numPlayers <- use numPlayersL
  return $ rotateLeft n $ map Index [0 .. numPlayers - 1]
