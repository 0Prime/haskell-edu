module Stepic.Step54Spec (spec) where

import Stepic.Step54
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "asToken" $ do
    context "fail" $ do
      it "abc isn't a Token" $ do
        asToken "abc" `shouldBe` Nothing

      it "'+1 isn't a Token" $ do
        asToken "+1" `shouldBe` Nothing

    context "success" $ do
      it "'+' is Plus" $ do
        asToken "+" `shouldBe` Just Plus

      it "'-' is Minus" $ do
        asToken "-" `shouldBe` Just Minus

      it "'(' is LeftBrace" $ do
        asToken "(" `shouldBe` Just LeftBrace

      it "')' is RightBrace" $ do
        asToken ")" `shouldBe` Just RightBrace

      it "42 is Number 42" $ do
        asToken "42" `shouldBe` Just (Number 42)

  describe "tokenize" $ do
    it "works 1" $ do
      tokenize "+" `shouldBe` Just [Plus]

    it "works 2" $ do
      tokenize "1 + ( 7 - 2 )"
        `shouldBe` Just [Number 1, Plus, LeftBrace, Number 7, Minus, Number 2, RightBrace]

  describe "pythagoreanTriple" $ do
    it "non-positive numbers return empty list" $ do
      pythagoreanTriple 0 `shouldBe` []
      pythagoreanTriple (-1) `shouldBe` []

    it "positive numbers work as expected" $ do
      pythagoreanTriple 4 `shouldBe` []
      pythagoreanTriple 5 `shouldBe` [(3, 4, 5)]
      pythagoreanTriple 6 `shouldBe` [(3, 4, 5)]
      pythagoreanTriple 9 `shouldBe` [(3, 4, 5)]
      pythagoreanTriple 10 `shouldBe` [(3, 4, 5), (6, 8, 10)]