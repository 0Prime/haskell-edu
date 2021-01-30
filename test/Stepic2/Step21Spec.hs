module Stepic2.Step21Spec (spec) where

import Stepic2.Step21
import Stepic2.Triple11
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Triple" $ do
    describe "is Foldable" $ do
      it "test 1" $ do
        foldr (++) "!!" (Tr "ab" "cd" "efg")
          `shouldBe` "abcdefg!!"

      it "test 2" $ do
        foldl (++) "!!" (Tr "ab" "cd" "efg")
          `shouldBe` "!!abcdefg"

  describe "Tree" $ do
    --   3
    --  / \
    -- 1   4
    --  \
    --   2
    let testTree =
          Branch
            (Branch Nil 1 (Branch Nil 2 Nil))
            3
            (Branch Nil 4 Nil)

    let verify f ex =
          foldr (:) [] (f testTree) `shouldBe` ex

    describe "is Foldable" $ do
      it "test 1" $ do
        verify id [1, 2, 3, 4]

      describe "Preorder" $ do
        it "test 1" $ do
          verify PreO [3, 1, 2, 4]

      describe "Postorder" $ do
        it "test 1" $ do
          verify PostO [2, 1, 4, 3]

      describe "Levelorder" $ do
        it "test 1" $ do
          verify LevelO [3, 1, 4, 2]