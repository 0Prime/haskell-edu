module Stepic2.Step31Spec (spec) where

import Stepic2.Step31
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Except" $ do
    it "smoke test" $ do
      'A' `shouldBe` 'A'