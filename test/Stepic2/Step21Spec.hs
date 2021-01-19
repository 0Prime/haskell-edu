module Stepic2.Step21Spec (spec) where

import Stepic2.Step21
import Stepic2.Triple11
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Triple" $ do
    describe "is Foldable" $ do
      it "test 1" $ do
        foldr (++) "!!" (Tr "ab" "cd" "efg")
          `shouldBe` "abcdefg!!"

      it "test 2" $ do
        foldl (++) "!!" (Tr "ab" "cd" "efg")
          `shouldBe` "!!abcdefg"
