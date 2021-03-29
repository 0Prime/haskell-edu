module Stepic2.Step25Spec (spec) where

import Stepic2.Step25
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "PrsEP" $ do
    let charEP c = satisfyEP (== c)

    it "test 1" $ do
      runPrsEP (charEP 'A') 0 "ABC"
        `shouldBe` (1, Right ('A', "BC"))

    it "test 2" $ do
      runPrsEP (charEP 'A') 41 "BCD"
        `shouldBe` (42, Left "pos 42: unexpected B")

    it "test 3" $ do
      runPrsEP (charEP 'A') 41 ""
        `shouldBe` (42, Left "pos 42: unexpected end of input")

    describe "parseEP" $ do
      it "test 1" $ do
        parseEP (charEP 'A') "ABC"
          `shouldBe` Right ('A', "BC")

      it "test 2" $ do
        parseEP (charEP 'A') "BCD"
          `shouldBe` Left "pos 1: unexpected B"

      it "test 3" $ do
        parseEP (charEP 'A') ""
          `shouldBe` Left "pos 1: unexpected end of input"