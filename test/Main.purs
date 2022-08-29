module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Joyride.Types (Column(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Column" do
          it "Has a correct semiring instance" do
            (C1 <> C4) `shouldEqual` C5
            (C1 <> C16) `shouldEqual` C1
            (C6 <> C3) `shouldEqual` C9
