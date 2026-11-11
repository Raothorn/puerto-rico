module Types.Role (Role (..)) where

data Role = Settler | Mayor | Builder | Craftsman | Trader | Prospector
  deriving (Eq, Ord, Show)
