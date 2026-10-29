{-# LANGUAGE RankNTypes #-}

module Util
  ( liftErr
  , liftMaybe
  , useMaybe
  , logEvent
  ) where

import Control.Monad.Trans.Class
import Lens.Micro
import Lens.Micro.Mtl

import Types

----------
-- Util --
----------
liftErr :: Error -> Update s a
liftErr = lift . Left

liftMaybe :: Error -> Maybe a -> Update s a
liftMaybe err x =
  case x of
    Just v -> return v
    Nothing -> liftErr err

useMaybe :: Error -> Lens' s (Maybe a) -> Update s a
useMaybe err l = do
  x <- use l
  liftMaybe err x

logEvent :: LogEvent -> Update GameState ()
logEvent event = eventLog %= (event :)
