module Stepic2.Step14Spec (spec) where

import Stepic2.Step14
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "stub description" $ do
    it "stub test" $ do
      1 `shouldBe` 1