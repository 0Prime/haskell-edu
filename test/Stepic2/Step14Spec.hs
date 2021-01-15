module Stepic2.Step14Spec (spec) where

import Control.Applicative
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

  describe "Prs as Functor" $ do
    it "test 1" $ do
      runPrs (digitToInt <$> anyChr) "BCD"
        `shouldBe` Just (11, "CD")

  describe "Prs as Applicative" $ do
    it "test 1" $ do
      runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE"
        `shouldBe` Just (('A', 'B', 'C'), "DE")

    it "test 2" $ do
      runPrs (anyChr *> anyChr) "ABCDE"
        `shouldBe` Just ('B', "CDE")

  describe "Prs as Alternative" $ do
    it "test 1" $ do
      runPrs (char 'A' <|> char 'B') "ABC"
        `shouldBe` Just ('A', "BC")

    it "test 2" $ do
      runPrs (char 'A' <|> char 'B') "BCD"
        `shouldBe` Just ('B', "CD")

    it "test 3" $ do
      runPrs (char 'A' <|> char 'B') "CDE"
        `shouldBe` Nothing

  describe "many1" $ do
    it "test1" $ do
      runPrs (many1 $ char 'A') "A"
        `shouldBe` Just ("A", "")

    it "test2" $ do
      runPrs (many1 $ char 'A') "AB"
        `shouldBe` Just ("A", "B")

    it "test3" $ do
      runPrs (many1 $ char 'A') "AA"
        `shouldBe` Just ("AA", "")

    it "test4" $ do
      runPrs (many1 $ char 'A') "AAA"
        `shouldBe` Just ("AAA", "")

    it "test5" $ do
      runPrs (many1 $ char 'A') "AAB"
        `shouldBe` Just ("AA", "B")

  describe "PrsE as Functor and Applicative" $ do
    let anyE = satisfyE (const True)

    it "test 1" $ do
      runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
        `shouldBe` Right (('A', 'C'), "DE")

    it "test 2" $ do
      runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
        `shouldBe` Left "unexpected B"

    it "test 3" $ do
      runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
        `shouldBe` Left "unexpected end of input"

  describe "charE" $ do
    it "test 1" $ do
      runPrsE (charE 'A') "ABC"
        `shouldBe` Right ('A', "BC")

    it "test 2" $ do
      runPrsE (charE 'A') "BCD"
        `shouldBe` Left "unexpected B"

    it "test 3" $ do
      runPrsE (charE 'A') ""
        `shouldBe` Left "unexpected end of input"