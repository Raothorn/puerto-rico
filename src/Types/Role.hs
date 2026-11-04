module Types.Role (Role (..)) where

data Role = Settler | Mayor | Builder | Craftsman | Trader
  deriving (Eq, Ord, Show)
