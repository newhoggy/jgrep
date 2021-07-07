{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module JGrep.Database
  ( Database
  , TypeName
  , Key
  , load
  ) where

import Control.Lens hiding (contains)
import Control.Monad
import Data.Aeson (Value)
import Data.Generics.Product.Any
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import JGrep.Errors (JGrepError (..), RenderError (..), panicOnLeftIO)
import JGrep.Schema (Schema, SchemaType)
import JGrep.Type (Key, TypeName, Database (Database), Table (Table), Indexes (Indexes))

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as J
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as DV
import qualified System.IO as IO
import qualified JGrep.Type as Z

{- HLINT ignore "Redundant return" -}

indexesForValue :: Int -> Value -> Indexes
indexesForValue idx value = Indexes
  { Z.exact = case value of
      J.Object hm -> HM.fromList $ uncurry exactEntry <$> HM.toList hm
      _ -> mempty
  , Z.contains = case value of
      J.Object hm -> foldr (HM.unionWith (<>)) mempty $ uncurry containsEntry <$> HM.toList hm
      _ -> mempty
  }
  where target = HS.singleton idx
        exactEntry :: Key -> Value -> ((Key, Value), HashSet Int)
        exactEntry k v = ((k, v), target)
        containsEntry :: Key -> Value -> HashMap (Key, Value) (HashSet Int)
        containsEntry k v = case v of
          J.Array es -> HM.fromList $ (, target) . (k, ) <$> DV.toList es
          _ -> mempty


load :: Schema -> IO Database
load schema = do
  let tt = schema ^. the @"types"
  tables <- forM tt $ \(schemaType :: SchemaType) -> do
    let typeName = schemaType ^. the @"name"
    records <- J.eitherDecodeFileStrict @[Value] (schemaType ^. the @"file")
      & panicOnLeftIO $ IO.hPutStrLn IO.stderr . renderError . JGrepErrorJsonError

    T.hPutStrLn IO.stderr $ "Loaded " <> (schemaType ^. the @"name") <> " from " <> T.pack (schemaType ^. the @"file")

    let recordsWithIndexList = L.zip @Int [0, 1 ..] records
    
    return
      ( typeName
      , Table
        { Z.records = DV.fromList records
        , Z.indexes = foldMap (uncurry indexesForValue) recordsWithIndexList
        }
      )

  return Database
    { Z.tables = HM.fromList tables
    }
