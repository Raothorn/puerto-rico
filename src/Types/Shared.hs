{-# LANGUAGE TemplateHaskell #-}

module Types.Shared
  ( Plantation (..)
  , Index (..)
  , Update
  , Role (..)
  )
where

import Control.Monad.Trans.State.Lazy
import Lens.Micro
import Lens.Micro.TH

import Types.Error

data Plantation = Tobacco | Sugar | Coffee | Corn | Indigo
  deriving (Eq, Ord)

data Role = Settler | Mayor | Builder | Craftsman | Trader

newtype Index a = Index Int
  deriving (Eq, Ord)

type Update s a = StateT s (Either Error) a
