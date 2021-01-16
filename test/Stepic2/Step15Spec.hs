module Stepic2.Step15Spec (spec) where

import Stepic2.Step15
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "stub" $ do
    it "test 1" $ do
      "A" `shouldBe` "A"