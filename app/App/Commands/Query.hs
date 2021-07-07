{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Query
  ( cmdQuery
  ) where

import Options.Applicative                                hiding (columns)

{- HLINT ignore "Reduce duplication" -}

runQuery :: () -> IO ()
runQuery _opts = return ()

optsQuery :: Parser ()
optsQuery = pure ()

cmdQuery :: Mod CommandFields (IO ())
cmdQuery = command "query"  $ flip info idm $ runQuery <$> optsQuery
