{-# LANGUAGE RankNTypes #-}
module StateUpdates
  (
  )
where

import Lens.Micro
import Lens.Micro.Mtl
import Control.Monad
import Control.Monad.Trans.Class
import Types

placePlantation :: (Index Player) -> Plantation -> Update GameState ()
placePlantation pIx plantation = 
    do
        remaining <- (plantationStacks . at plantation)
            <%= fmap (subtract 1) 
            >>= liftMaybe NotFoundError
        when (remaining < 0) $
            liftErr NoPlantationsLeftInStack

        zoom (playerL pIx . board) $ do
            plantations' <- plantations <%= (plantation:)
            when (length plantations' > 12) $
                liftErr NoEmptyPlantationTiles

----------
-- Util --
----------
-- zoomPlayer :: Index Player -> Update GameState Player -> Update Player 
-- zoomPlayer pIx = useMaybe NoPlayerFound (playerL pIx)

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
