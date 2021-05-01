module Stepic2.Step32Spec (spec) where

import Stepic2.Step32
import Test.Hspec
import Test.Hspec.Tables

spec :: Spec
spec = parallel $ do
  describe "number function" $ do
    byExample
      ("values", "expected")
      [ ([1], 1),
        ([2], 2),
        ([1, 20], 21),
        ([100, 1], 100),
        ([3, 20, 100, 1], 123),
        ([1000, 1], 1000),
        ([1, 1000, 1], 1001),
        ([2, 2000], 2002),
        ([1, 1000, 2], 2001)
      ]
      (\xs e -> number xs `shouldBe` e)

  describe "decode function" $ do
    byExample
      ("values", "expected")
      [ (decode one number, 1),
        (decode one as a number, 1),
        (decode one hundred twenty three as a number, 123),
        (decode one hundred twenty one number, 121),
        (decode one hundred twenty number, 120),
        (decode one hundred number, 100),
        (decode three hundred number, 300),
        (decode two thousand one number, 2001),
        (decode two thousand seventeen number, 2017)
      ]
      shouldBe
