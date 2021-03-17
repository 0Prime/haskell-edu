module Stepic2.Step24Spec (spec) where

import Stepic2.Step14
import Stepic2.Step24
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "PrsE" $ do
    describe "is Monad" $ do
      let testHelper = runPrsE $ do
            a <- charE 'A'
            b <- charE 'B'
            return (a, b)

      it "test 1" $ do
        testHelper "ABC" `shouldBe` Right (('A', 'B'), "C")

      it "test 2" $ do
        testHelper "ACD" `shouldBe` Left "unexpected C"

      it "test 3" $ do
        testHelper "BCD" `shouldBe` Left "unexpected B"
