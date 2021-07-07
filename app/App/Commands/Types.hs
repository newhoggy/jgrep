{-# LANGUAGE DeriveGeneric #-}

module App.Commands.Types
  ( QueryOpts (..)
  ) where

import GHC.Generics (Generic)

newtype QueryOpts = QueryOpts
  { schema :: FilePath
  } deriving (Eq, Show, Generic)
