module Stepic2.Step23Spec (spec) where

import Data.Foldable
import Data.Traversable (foldMapDefault)
import Lib (Tree (..))
import Stepic2.Step23
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "OddC" $ do
    let cnt1 = Un 42
    let cnt3 = Bi 1 2 cnt1
    let cnt5 = Bi 3 4 cnt3
    let cntInf = Bi 'A' 'B' cntInf

    describe "is Functor" $ do
      it "test 1" $ do
        (+ 1) <$> cnt5
          `shouldBe` Bi 4 5 (Bi 2 3 (Un 43))

    describe "is Foldable" $ do
      it "test 1" $ do
        toList cnt5
          `shouldBe` [3, 4, 1, 2, 42]

      it "test 2" $ do
        sum cnt5 `shouldBe` 52

    describe "is Traversable" $ do
      it "test 1" $ do
        traverse (\x -> [x + 2, x -2]) cnt1
          `shouldBe` [Un 44, Un 40]

    describe "Temperature" $ do
      describe "Kelvin to Celsius (k2c)" $ do
        it "test 1" $ do
          k2c 0 `shouldBe` Temperature (-273.15)

        it "test 2" $ do
          k2c 0 == Temperature (-273.15) `shouldBe` True

        it "test 3" $ do
          k2c 273.15 `shouldBe` Temperature 0.0

  describe "Tree" $ do
    describe "is Traversable" $ do
      let testTree =
            Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

      it "test 1" $ do
        foldMapDefault (: []) testTree
          `shouldBe` [1, 3, 2, 5, 4]
