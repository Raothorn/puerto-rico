{-# LANGUAGE RankNTypes #-}

module Util
  ( liftErr
  , liftMaybe
  , zoomRequired
  , useMaybe
  , logEvent
  , removeAt
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
    Just value -> return value
    Nothing -> liftErr err

zoomRequired :: Error -> Traversal' s a -> Update a () -> Update s ()
zoomRequired err target action = do
  _ <- preuse target >>= liftMaybe err
  zoom target action

-- | Read an optional value through a lens, failing when it is absent.
useMaybe :: Error -> SimpleGetter s (Maybe a) -> Update s a
useMaybe err valueLens = do
  maybeValue <- use valueLens
  liftMaybe err maybeValue

-- | Prepend an event to the game log.
logEvent :: LogEvent -> Update GameState ()
logEvent event = eventLog %= (event :)

---------------
-- List Util --
---------------
removeAt :: (Index a) -> [a] -> Maybe (a, [a])
removeAt (Index i) xs
  | i < 0 = Nothing
  | otherwise =
      case splitAt i xs of
        (before, x : after) -> Just (x, before ++ after)
        (_, []) -> Nothing
