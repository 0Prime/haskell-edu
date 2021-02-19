module Stepic2.Step22Spec (spec) where

import Stepic2.Step21
import Stepic2.Step22
import Stepic2.Triple11
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "traverse2list" $ do
    it "works with list" $ do
      traverse2list (\x -> [x + 10, x + 20]) [1, 2, 3]
        `shouldBe` [ [11, 12, 13],
                     [11, 12, 23],
                     [11, 22, 13],
                     [11, 22, 23],
                     [21, 12, 13],
                     [21, 12, 23],
                     [21, 22, 13],
                     [21, 22, 23]
                   ]

    it "works with tree" $ do
      traverse2list
        (\x -> [x + 10, x + 20])
        (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil))
        `shouldBe` [ [11, 12, 13],
                     [11, 12, 23],
                     [11, 22, 13],
                     [11, 22, 23],
                     [21, 12, 13],
                     [21, 12, 23],
                     [21, 22, 13],
                     [21, 22, 23]
                   ]

  describe "Triple as Traversable" $ do
    it "test 1" $ do
      foldl (++) "!!" (Tr "ab" "cd" "efg")
        `shouldBe` "!!abcdefg"

    it "test 2" $ do
      traverse (\x -> if x > 10 then Right x else Left x) (Tr 12 14 16)
        `shouldBe` Right (Tr 12 14 16)

    it "test 3" $ do
      traverse (\x -> if x > 10 then Right x else Left x) (Tr 12 8 4)
        `shouldBe` Left 8

    it "test 4" $ do
      sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
        `shouldBe` Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)