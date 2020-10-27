module Stepic.State58Spec (spec) where

import Stepic.State58
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "numberTree" $ do
    it "simple case" $ do
      numberTree (Leaf ()) `shouldBe` Leaf 1

    it "complex case" $ do
      numberTree (Fork (Leaf ()) () (Leaf ())) `shouldBe` Fork (Leaf 1) 2 (Leaf 3)