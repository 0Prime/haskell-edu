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