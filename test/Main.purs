module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Joyride.Types (Column(..), Version, Whitelist(..))
import Simple.JSON as JSON
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $
      runSpec [ consoleReporter ] do
        describe "Column" do
          it "Has a correct semiring instance" do
            (C1 <> C4) `shouldEqual` C5
            (C1 <> C16) `shouldEqual` C1
            (C6 <> C3) `shouldEqual` C9
        describe "Track" do
          it "Parses correctly" do
            ( JSON.readJSON
                """{ "url": "foo"
  , "private": false
  , "owner": "me"
  , "version": 0
  }
"""
            ) `shouldEqual` (Right { url: "foo", owner: "me", private: false, title: Nothing :: Maybe String, whitelist: Whitelist [], version: mempty :: Version 0 })
