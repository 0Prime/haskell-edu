module Stepic.State58Spec (spec) where

import Stepic.State58
import Test.Hspec

spec :: Spec
spec = parallel $ do
  let test1 f = it "simple case" $ do
        f (Leaf ())
          `shouldBe` Leaf 1

  let test2 f = it "complex case" $ do
        f (Fork (Leaf ()) () (Leaf ()))
          `shouldBe` Fork (Leaf 1) 2 (Leaf 3)

  describe "numberTree" $ do
    test1 numberTree
    test2 numberTree

  describe "numberTree'" $ do
    test1 numberTree'
    test2 numberTree'