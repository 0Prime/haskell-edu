module Stepic2.Step32Spec (spec) where

import Stepic2.Step32
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "decode function" $ do
    it "test 1" $ do
      'a' `shouldBe` 'a'