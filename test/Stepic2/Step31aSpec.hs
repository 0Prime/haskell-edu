module Stepic2.Step31aSpec (spec) where

import Control.Monad.Trans.Except
import Stepic2.Step31a
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "!!! operator" $ do
    it "test 1" $ do
      runExcept ([1 .. 100] !!! 5)
        `shouldBe` Right 6

    let (!!!!) xs n = runExcept $ xs !!! n

    it "test 2" $ do
      [1, 2, 3] !!!! 0
        `shouldBe` Right 1

    it "test 3" $ do
      [1, 2, 3] !!!! 42
        `shouldBe` Left (ErrIndexTooLarge 42)

    it "test 4" $ do
      [1, 2, 3] !!!! (-3)
        `shouldBe` Left ErrNegativeIndex

  describe "tryRead function" $ do
    it "test 1" $ do
      runExcept (tryRead "5" :: Except ReadError Int)
        `shouldBe` Right 5

    it "test 2" $ do
      runExcept (tryRead "5" :: Except ReadError Double)
        `shouldBe` Right 5.0

    it "test 3" $ do
      runExcept (tryRead "5zzz" :: Except ReadError Int)
        `shouldBe` Left (NoParse "5zzz")

    it "test 4" $ do
      runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ()))
        `shouldBe` Right (True, ())

    it "test 5" $ do
      runExcept (tryRead "" :: Except ReadError (Bool, ()))
        `shouldBe` Left EmptyInput

    it "test 6" $ do
      runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))
        `shouldBe` Left (NoParse "wrong")

  describe "trySum function" $ do
    let testWith val = runExcept (trySum val :: Except SumError Integer)

    it "test 1" $ do
      testWith ["10", "20", "30"]
        `shouldBe` Right 60

    it "test 2" $ do
      testWith ["10", "20", ""]
        `shouldBe` Left (SumError 3 EmptyInput)

    it "test 3" $ do
      testWith ["10", "two", "30"]
        `shouldBe` Left (SumError 2 (NoParse "two"))