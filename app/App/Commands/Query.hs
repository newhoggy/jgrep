{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Query
  ( cmdQuery
  ) where

import App.Commands.Types (QueryOpts (QueryOpts))
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import JGrep.Errors (JGrepError (..), RenderError (..), panicOnLeftIO)
import JGrep.Schema (Schema)
import Options.Applicative                                hiding (columns)

import qualified Data.Yaml as Y
import qualified JGrep.Database as DB
import qualified System.IO as IO

{- HLINT ignore "Reduce duplication" -}

runQuery :: QueryOpts -> IO ()
runQuery opts = do
  schema :: Schema <- Y.decodeFileEither (opts ^. the @"schema")
    & panicOnLeftIO $ IO.hPutStrLn IO.stderr . renderError . JGrepErrorYamlError

  void $ DB.load schema

  return ()

optsQuery :: Parser QueryOpts
optsQuery = QueryOpts
  <$> strOption
        (   long "schema"
        <>  short 's'
        <>  help "Schema file"
        <>  metavar "FILE"
        )

cmdQuery :: Mod CommandFields (IO ())
cmdQuery = command "query"  $ flip info idm $ runQuery <$> optsQuery
