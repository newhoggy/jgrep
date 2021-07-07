{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Demo
  ( cmdDemo
  ) where

import Options.Applicative                                hiding (columns)

{- HLINT ignore "Reduce duplication" -}

runDemo :: () -> IO ()
runDemo _opts = return ()

optsDemo :: Parser ()
optsDemo = pure ()

cmdDemo :: Mod CommandFields (IO ())
cmdDemo = command "demo"  $ flip info idm $ runDemo <$> optsDemo
