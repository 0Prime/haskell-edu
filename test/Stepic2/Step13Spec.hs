module Stepic2.Step13Spec (spec) where

import Stepic2.Step13
import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

spec :: Spec
spec = parallel $ do
  describe "getList" $ do
    let getListTest = parse getList ""

    it "1 is [1]" $ do
      getListTest "1" `shouldParse` ["1"]

    it "1;234;56 is [1,234,56]" $ do
      getListTest "1;234;56" `shouldParse` ["1", "234", "56"]

    it "fails on trailing ';'" $ do
      getListTest `shouldFailOn` "1;234;56;"

    it "fails on double ';'" $ do
      getListTest `shouldFailOn` "1;;234;56"

  describe "ignoreBraces" $ do
    let lBrace = string "[["
        rBrace = string "]]"
        body = many1 letter
        ignoreBracesTest = parse (ignoreBraces lBrace rBrace body) ""

    it "test 1" $ do
      ignoreBracesTest "[[ABC]]DEF" `shouldParse` "ABC"