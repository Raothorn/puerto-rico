{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.ActionTest (runTests) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Lens.Micro
import Lens.Micro.Mtl
import Test.Tasty
import Test.Tasty.HUnit

import ExecAction
import Test.Helpers
import Types

type TestCase = StateT GameState IO ()

numPlayers :: Int
numPlayers = 4

sampleGame :: TestCase
sampleGame = do
  -- Check initial state
  playerCount <- use numPlayersL
  liftIO $ assertEqual "There should be 4 players" 4 playerCount

  zoom (players . traversed) $ do
    playerDoubloons <- use doubloons
    liftIO $ assertEqual "Each player should have 3 doubloons" 3 playerDoubloons

  playerPlantations <- gets $ toListOf (players . traversed . tiles . traversed . tileType)
  liftIO $
    assertEqual
      "The starting plantations should be: Indigo, Indigo, Corn Corn"
      (map PlantationTile [Indigo, Indigo, Corn, Corn])
      playerPlantations

runTests :: TestTree
runTests =
  testGroup
    "action tests"
    [ testCase "Sample game" $ evalStateT sampleGame game
    , testGroup
        "Initial player order"
        [ testCase (show count ++ " players") $ initialPlayerOrder count
        | count <- [3 .. 5]
        ]
    , testGroup
        "Settler player indices"
        [ testCase ("Player index " ++ show playerIx) $ settlerPlayerIndex playerIx
        | playerIx <- [0 .. numPlayers - 1]
        ]
    ]
 where
  game = newGame numPlayers

initialPlayerOrder :: Int -> IO ()
initialPlayerOrder count = do
  let game = newGame count
  assertEqual "The player list should have the requested length" count (length (game ^. players))
  case game ^. phaseStack of
    [ChooseRolePhase _ playerIxs] ->
      assertEqual "Turn order should use zero-based list positions" (map Index [0 .. count - 1]) playerIxs
    phases -> assertFailure $ "Expected the initial role selection phase, got: " ++ show phases

settlerPlayerIndex :: Int -> IO ()
settlerPlayerIndex playerIx = evalStateT test (newGame numPlayers)
 where
  test = do
    startingTiles <- gets $ map (toListOf (tiles . traversed . tileType)) . (^. players)
    faceUpPlantations .= [Coffee]
    step $ execRoleAction (Index playerIx) False (SettlerAction (TakePlantationAction (Index 0)))
    updatedTiles <- gets $ map (toListOf (tiles . traversed . tileType)) . (^. players)
    let expectedTiles =
          [ if i == playerIx then PlantationTile Coffee : playerTiles else playerTiles
          | (i, playerTiles) <- zip [0 ..] startingTiles
          ]
    liftIO $ assertEqual "Only the player at the requested list position should receive the plantation" expectedTiles updatedTiles

step :: Update s a -> StateT s IO a
step action = StateT $ \state ->
  case runStateT action state of
    Left err -> assertFailure (show err)
    Right result -> return result

expectErr :: Error -> Update s a -> StateT s IO ()
expectErr expectedErr action = StateT $ \state ->
  case runStateT action state of
    Left err -> do
      assertEqual "Expected error" expectedErr err
      pure ((), state)
    Right _ -> assertFailure failureString
 where
  failureString =
    "Expected error: "
      <> show expectedErr
      <> " but got valid result."
