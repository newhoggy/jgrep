module JGrep.CoreSpec where

import HaskellWorks.Hspec.Hedgehog
import Test.Hspec

{- HLINT ignore "Redundant do" -}

spec :: Spec
spec = describe "JGrep.CoreSpec" $ do
  it "SimpleTest that always succeeds" $ requireTest $ do
    return ()
