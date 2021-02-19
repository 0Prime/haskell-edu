module Stepic2.Step22Spec (spec) where

import Stepic2.Step21
import Stepic2.Step22
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