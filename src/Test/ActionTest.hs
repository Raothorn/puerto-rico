{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.ActionTest (runTests) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Maybe
import Lens.Micro
import Lens.Micro.Mtl
import Test.Tasty
import Test.Tasty.HUnit

import Types

type TestCase = StateT GameState IO ()

numPlayers :: Int
numPlayers = 4

sampleGame :: TestCase
sampleGame = do
  -- Check initial state
  playerCount <- use numPlayersL
  liftIO $ assertEqual "There should be 4 players" 4 playerCount

  zoom (gamePlayers . traversed) $ do
    playerDoubloons <- use doubloons
    liftIO $ assertEqual "Each player should have 3 doubloons" 3 playerDoubloons

  playerPlantations <- gets $ toListOf (gamePlayers . traversed . tiles . traversed . tileType)
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
    ]
 where
  game = fromJust $ newGame numPlayers

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
