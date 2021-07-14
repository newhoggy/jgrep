{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}

module JGrep.Schema
  ( SchemaType (..)
  , Fields (..)
  , Schema(..)
  ) where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

newtype Schema = Schema
  { types :: [SchemaType]
  } deriving (Eq, Show, Generic)

deriving instance ToJSON Schema
deriving instance FromJSON Schema

data Fields = Fields
  { name  :: Text
  , key   :: Text
  } deriving (Eq, Show, Generic)

deriving instance ToJSON Fields
deriving instance FromJSON Fields

data SchemaType = SchemaType
  { name    :: Text
  , file    :: FilePath
  , fields  :: [Fields]
  } deriving (Eq, Show, Generic)

deriving instance ToJSON SchemaType
deriving instance FromJSON SchemaType
