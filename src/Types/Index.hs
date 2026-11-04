module Types.Index (Index (..)) where

newtype Index a = Index Int
  deriving (Eq, Ord, Show)
