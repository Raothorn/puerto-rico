module Main (main) where

import Actions (execRoleAction)
import Control.Monad.Trans.State.Lazy (runStateT)
import qualified Data.Map as M
import Lens.Micro ((^.), (^?))
import Test.HUnit
  ( Assertion
  , Test (TestCase, TestLabel)
  , assertBool
  , assertEqual
  , assertFailure
  , runTestTTAndExit
  )
import Types

main :: IO ()
main = runTestTTAndExit $
  TestLabel "settler places a plantation and records the action" $
    TestCase settlerHappyPath

settlerHappyPath :: Assertion
settlerHappyPath = do
  let actingPlayer = Index 1
      secondPlayer = Index 2
      thirdPlayer = Index 3
      actingBoard = PlayerBoard [Sugar] [Index 7] 4
      secondBoard = PlayerBoard [Tobacco] [] 2
      thirdBoard = PlayerBoard [Quarry] [] 3
      initialState = GameState
        { _phase = RoleActionPhase [Mayor] []
        , _players = M.fromList
            [ (actingPlayer, Player actingBoard)
            , (secondPlayer, Player secondBoard)
            , (thirdPlayer, Player thirdBoard)
            ]
        , _plantationStacks = M.fromList
            [(Coffee, 3), (Sugar, 4), (Tobacco, 5), (Quarry, 6)]
        , _colonistShip = 12
        , _eventLog = [NoEvent]
        }

  case runStateT (execRoleAction actingPlayer (SettlerAction Coffee)) initialState of
    Left _ -> assertFailure "Expected the settler action to succeed"
    Right ((), finalState) -> do
      assertBool "Only the selected plantation stack is decremented" $
        finalState ^. plantationStacks == M.fromList
          [(Coffee, 2), (Sugar, 4), (Tobacco, 5), (Quarry, 6)]

      assertBoard "Acting player" (PlayerBoard [Coffee, Sugar] [Index 7] 4) $
        finalState ^? playerL actingPlayer . board
      assertBoard "Second player" secondBoard $
        finalState ^? playerL secondPlayer . board
      assertBoard "Third player" thirdBoard $
        finalState ^? playerL thirdPlayer . board
      assertEqual "All three players remain in the game" 3 $
        finalState ^. numPlayersL
      assertEqual "The colonist ship is unchanged" 12 (_colonistShip finalState)

      case finalState ^. eventLog of
        [PlantationPlaced loggedPlayer Coffee, NoEvent] ->
          assertBool "The placement event identifies the acting player" $
            loggedPlayer == actingPlayer
        _ -> assertFailure "Expected one coffee placement event before the existing log"

      case finalState ^. phase of
        RoleActionPhase [Settler, Mayor] [finishedPlayer] ->
          assertBool "The acting player is marked as finished" $
            finishedPlayer == actingPlayer
        _ -> assertFailure "Expected the role action phase to record Settler and continue"

assertBoard :: String -> PlayerBoard -> Maybe PlayerBoard -> Assertion
assertBoard label expected actual =
  case actual of
    Nothing -> assertFailure (label ++ ": player is missing")
    Just actualBoard -> do
      assertBool (label ++ ": plantations match in order") $
        _plantations actualBoard == _plantations expected
      assertBool (label ++ ": buildings are unchanged") $
        _buildings actualBoard == _buildings expected
      assertEqual (label ++ ": dubloons are unchanged")
        (_dubloons expected) (_dubloons actualBoard)
