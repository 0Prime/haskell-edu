module Stepic.Maybe1Spec (spec) where

import Stepic.Maybe1
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Monoid laws" $ do
    it "mempty is not Nothing" $ do
      (mempty :: Maybe' String) `shouldNotBe` nothing

    it "mempty is something" $ do
      (mempty :: Maybe' String) `shouldBe` Maybe' (Just "")

    it "left Nothing is Nothing" $ do
      nothing <> mempty `shouldBe` nothing

    it "right Noting is Nothing" $ do
      mempty <> nothing `shouldBe` nothing

    it "double Nothing is Nothing" $ do
      nothing <> nothing `shouldBe` nothing

    it "Right identity" $ do
      just "x" <> mempty `shouldBe` just "x"

    it "Left identity" $ do
      mempty <> just "x" `shouldBe` just "x"

    it "Associativity" $ do
      just "x" <> (just "y" <> just "z") `shouldBe` (just "x" <> just "y") <> just "z"

    let testConcatenation xs = it ("Concatenation test on: " ++ show xs) $ do
          mconcat jxs `shouldBe` foldr mappend mempty jxs
          where
            jxs = map just xs

    testConcatenation ["x"]
    testConcatenation ["x", "y"]
    testConcatenation ["y", "x"]
    testConcatenation ["x", "y", "z"]

nothing :: Maybe' String
nothing = Maybe' Nothing

just :: String -> Maybe' String
just x = Maybe' $ Just x