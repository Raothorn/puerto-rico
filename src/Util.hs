{-# LANGUAGE RankNTypes #-}

module Util
  ( liftErr
  , liftMaybe
  , useMaybe
  , logEvent
  )
where

import Control.Monad.Trans.Class
import Lens.Micro
import Lens.Micro.Mtl

import Types

-- | Fail an update with the supplied error.
liftErr :: Error -> Update s a
liftErr = lift . Left

-- | Unwrap a value, failing the update when it is absent.
liftMaybe :: Error -> Maybe a -> Update s a
liftMaybe err maybeValue =
  case maybeValue of
    Just value -> pure value
    Nothing -> liftErr err

-- | Read an optional value through a lens, failing when it is absent.
useMaybe :: Error -> Lens' s (Maybe a) -> Update s a
useMaybe err valueLens = do
  maybeValue <- use valueLens
  liftMaybe err maybeValue

-- | Prepend an event to the game log.
logEvent :: LogEvent -> Update GameState ()
logEvent event = eventLog %= (event :)
