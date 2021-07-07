{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module JGrep.CoreSpec where

import Control.Lens ((^?))
import Data.Aeson (ToJSON(..))
import Data.Function ((&))
import Data.Maybe
import HaskellWorks.Hspec.Hedgehog
import Test.Hspec

import JGrep.Errors
import JGrep.Query
import JGrep.Schema
import JGrep.Type

import Hedgehog ((===))

import qualified Data.Aeson.Lens as CL
import qualified Data.List as L
import qualified Data.Yaml as Y
import qualified JGrep.Database as DB
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

{- HLINT ignore "Redundant do" -}

db :: Database
db = IO.unsafePerformIO $ do
  schema :: Schema <- Y.decodeFileEither "schema.yaml"
    & panicOnLeftIO $ IO.hPutStrLn IO.stderr . renderError . JGrepErrorYamlError
  DB.load schema
{-# NOINLINE db #-}

spec :: Spec
spec = describe "JGrep.CoreSpec" $ do
  it "Single record lookup field equality query" $ requireTest $ do
    let results = runQuery (SelectQuery "users" (Eq (toJSON @Int 1) "_id")) db
    
    mapMaybe (^? CL.key "external_id") results === fmap (toJSON @String) ["74341f74-9c79-49d5-9611-87ef9b6eb75f"]
  it "Multiple record lookup field equality query" $ requireTest $ do
    let results = runQuery (SelectQuery "users" (Eq (toJSON @String "en-AU") "locale")) db

    L.sort (mapMaybe (^? CL.key "external_id") results) === fmap (toJSON @String)
      -- Expected value from:
      --
      -- cat data/users.json | jq -r '.[] | select(."locale" == "en-AU") | ."external_id"' | sort
      [ "0320fff9-8a1f-4654-a8d5-dbf3c9c79591"
      , "08a71e65-dd92-4774-966a-876766c0f962"
      , "1c8b9696-b5c5-4f5a-b077-5d85dd8b22f4"
      , "379a17bc-ee5f-4783-b0c0-34de7a63d9d7"
      , "38899b1e-89ca-43e7-b039-e3c88525f0d2"
      , "3abbffbf-eb9d-4738-825b-cf3c3e76f215"
      , "3f693339-8c05-4619-b60e-f4892b42f340"
      , "46c880dd-d07e-4fd0-87d0-e92c167d029e"
      , "497def74-ce88-442e-814c-2fb8ce945dd9"
      , "5cf7c032-b3cb-4c87-afa1-57fc9f94e9a1"
      , "66a3589c-8aab-485b-9da3-e79adb85a4de"
      , "6c1ee6e7-060b-4ceb-8729-e80cc6dcab66"
      , "72c7ba23-e070-4583-b701-04a038a28b02"
      , "74341f74-9c79-49d5-9611-87ef9b6eb75f"
      , "80e6a7b7-9a2a-4b93-93da-68bd5e95cbb8"
      , "85c599c1-ebab-474d-a4e6-32f1c06e8730"
      , "8fa4f74b-e690-4478-bf09-40fed1ebc417"
      , "95387bef-5870-4453-9431-be6f9864bad8"
      , "99006a7e-3279-416d-a2f2-d92aca820ec0"
      , "9d8577b6-bffa-4abf-8bd5-c3b673dd0855"
      , "a8b6c657-d47e-45b2-9c47-cf13b1b02f24"
      , "af6ff47c-1ca0-45b1-b7d9-598ff5147a0d"
      , "b7a414da-3766-41cf-9a22-a8c37523285f"
      , "bce94e82-b4f4-438f-bc0b-2440e8265705"
      , "bf313bac-e4b1-46a3-a7ee-7cc584b7cbb8"
      , "c861e0ec-543d-46a3-8e9b-b01a0131be37"
      , "dce36d80-f143-4b1e-8a2e-47651e367a4e"
      , "e29c3611-d1f2-492e-a805-594e239ff922"
      , "e9db9277-af4a-4ca6-99e0-291c8a97623e"
      , "ee53ec4a-8ae1-4090-8f27-ce511cc292f7"
      , "f744706e-df3d-4d51-ad18-c63e41be5cc0"
      , "feacbd09-4aed-4c45-b9e0-af3898277cb3"
      ]
  it "Multiple record lookup field equality query with Or" $ requireTest $ do
    let results = runQuery (SelectQuery "users" (Or (Eq (toJSON @Int 1) "_id") (Eq (toJSON @Int 10) "_id"))) db

    L.sort (mapMaybe (^? CL.key "external_id") results) === fmap (toJSON @String)
      -- Expected value from:
      --
      -- cat data/users.json | jq -r '.[] | select(."_id" == 1 or ."_id" == 10) | ."external_id"' | sort
      [ "74341f74-9c79-49d5-9611-87ef9b6eb75f"
      , "f744706e-df3d-4d51-ad18-c63e41be5cc0"
      ]
  it "Multiple record lookup field equality query with Or" $ requireTest $ do
    let results = runQuery (SelectQuery "users" (And (Eq (toJSON True) "active") (And (Eq (toJSON True) "verified") (Eq (toJSON True) "shared")))) db

    L.sort (mapMaybe (^? CL.key "external_id") results) === fmap (toJSON @String)
      -- Expected value from:
      --
      -- cat data/users.json | jq -r '.[] | select(.active == true and .verified == true and .shared == true) | ."external_id"' | sort
      [ "37c9aef5-cf01-4b07-af24-c6c49ac1d1c7"
      , "981adbf1-02cd-4ff4-83f7-b6768abb25e5"
      , "9d8577b6-bffa-4abf-8bd5-c3b673dd0855"
      , "e29c3611-d1f2-492e-a805-594e239ff922"
      , "f844d39b-1d2c-4908-8719-48b5930bc6a2"
      ]
  it "Multiple record lookup field contains query" $ requireTest $ do
    let results = runQuery (SelectQuery "tickets" (And (In (toJSON @String "New York") "tags") (In (toJSON @String "Minnesota") "tags"))) db

    L.sort (mapMaybe (^? CL.key "external_id") results) === fmap (toJSON @String)
      -- Expected value from:
      --
      -- cat data/tickets.json | jq -r '.[] | select((.tags[] | contains("New York")) and (.tags[] | contains("Minnesota"))) | .external_id' | sort
      [ "12ccfd3b-0aaf-4c19-bacb-6d999c4fc5b0"
      , "3db2c1e6-559d-4015-b7a4-6248464a6bf0"
      , "3e690d6e-b322-4d56-b72a-a18cda46f717"
      , "4050fefa-f86f-4254-8ee3-dee3e534ab12"
      , "701c4f81-24e7-40c6-90f6-d8653a305c00"
      , "a83f28f4-6654-4d3a-9b58-c05d7a9b3d2d"
      , "f3e1924b-8dee-4f3c-907a-89edd2250523"
      ]
