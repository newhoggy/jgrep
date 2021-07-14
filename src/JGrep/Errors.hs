{-# LANGUAGE LambdaCase #-}

module JGrep.Errors
  ( JGrepError (..)
  , RenderError (..)

  , panicOnLeft
  , panicOnLeftIO
  ) where

import Data.Yaml (ParseException)

import qualified System.Exit as IO

data JGrepError
  = JGrepErrorYamlError ParseException
  | JGrepErrorJsonError String
  deriving Show

class RenderError e where
  renderError :: e -> String

instance RenderError JGrepError where
  renderError = \case
    JGrepErrorYamlError e -> "Unable to Parse YAML: " <> show e
    JGrepErrorJsonError e -> "Unable to Parse JSON: " <> show e

panicOnLeft :: Either e a -> (e -> IO ()) -> IO a
panicOnLeft result handler = case result of
  Right a -> return a
  Left e -> do
    handler e
    IO.exitFailure

panicOnLeftIO :: IO (Either e a) -> (e -> IO ()) -> IO a
panicOnLeftIO f handler = f >>= flip panicOnLeft handler
