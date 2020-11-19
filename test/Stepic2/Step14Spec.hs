module Stepic2.Step14Spec (spec) where

import Data.Char
import Stepic2.Step14
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "anyChr" $ do
    it "test 1" $ do
      runPrs anyChr "A" `shouldBe` Just ('A', "")

    it "test 1" $ do
      runPrs anyChr "ABC" `shouldBe` Just ('A', "BC")

    it "test 3" $ do
      runPrs anyChr "" `shouldBe` Nothing

    it "test 4" $ do
      runPrs (digitToInt <$> anyChr) "BCD"
        `shouldBe` Just (11, "CD")

  describe "Prs as Applicative" $ do
    it "test 1" $ do
      runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE"
        `shouldBe` Just (('A', 'B', 'C'), "DE")

    it "test 2" $ do
      runPrs (anyChr *> anyChr) "ABCDE"
        `shouldBe` Just ('B', "CDE")