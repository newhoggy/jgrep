{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module JGrep.Type
  ( TypeName
  , Key

  , Database(..)
  , Table(..)
  , Indexes(..)

  , Selector(..)
  , Source(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import qualified Data.HashMap.Lazy as HM

type TypeName = Text
type Key = Text

data Selector = Selector
  { type_ :: TypeName
  , field :: Text
  , alias :: Text
  } deriving (Eq, Show, Generic)

data Source = Source
  { type_ :: TypeName
  , alias :: Text
  } deriving (Eq, Show, Generic)

newtype Database = Database
  { tables :: HashMap TypeName Table
  } deriving (Eq, Show, Generic)

data Table = Table
  { records :: Vector Value
  , indexes :: Indexes
  } deriving (Eq, Show, Generic)

data Indexes = Indexes
  { exact :: HashMap (Key, Value) (HashSet Int)
  , contains :: HashMap (Key, Value) (HashSet Int)
  } deriving (Eq, Show, Generic)

instance Semigroup Indexes where
  a <> b = Indexes
    { exact = HM.unionWith (<>) (exact a) (exact b)
    , contains = HM.unionWith (<>) (contains a) (contains b)
    }

instance Monoid Indexes where
  mempty = Indexes mempty mempty
