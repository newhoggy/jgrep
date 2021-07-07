{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module JGrep.Query
  ( runQuery
  , Filter (..)
  , Query (..)
  ) where

import Control.Lens ( (^.))
import Data.Aeson (Value)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import JGrep.Database
import Data.Generics.Product.Any (HasAny(the))
import JGrep.Type (Table)
import Prelude hiding (filter)

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as DV

type Field = Text

data Filter
  = Eq Value Field
  | In Value Field
  | And Filter Filter
  | Or Filter Filter

data Query = SelectQuery TypeName Filter

runQuery :: Query -> Database -> [Value]
runQuery query db = case query of
  SelectQuery typeName filter -> case HM.lookup typeName (db ^. the @"tables") of
    Just table -> ((table ^. the @"records") DV.!) <$> HS.toList (runFilter table filter)
    Nothing -> []

runFilter :: Table -> Filter -> HashSet Int
runFilter table filter = case filter of
  Eq value field -> fromMaybe mempty $ HM.lookup (field, value) (table ^. the @"indexes" . the @"exact")
  In value field -> fromMaybe mempty $ HM.lookup (field, value) (table ^. the @"indexes" . the @"contains")
  And lt rt -> HS.intersection (runFilter table lt) (runFilter table rt)
  Or lt rt -> HS.union (runFilter table lt) (runFilter table rt)
