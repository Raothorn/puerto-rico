module Types.Index (Index (..), mapIndex) where

newtype Index a = Index Int
  deriving (Eq, Ord, Show)

mapIndex :: (Int -> Int) -> Index a -> Index a
mapIndex f (Index n) = Index (f n)
