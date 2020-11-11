{-# LANGUAGE RankNTypes #-}

module Stepic2.Step12Spec (spec) where

import Control.Applicative (ZipList (ZipList))
import Stepic2.Step12
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe ">*< and >$< functions" $ do
    let x1s = [1, 2, 3]
    let x2s = [4, 5, 6]
    let x3s = [7, 8, 9]
    let x4s = [10, 11, 12]

    it "test 1" $ do
      (\a b -> 2 * a + 3 * b)
        >$< x1s >*< x2s
          `shouldBe` [14, 19, 24]

    it "test 2" $ do
      (\a b c -> 2 * a + 3 * b + 5 * c)
        >$< x1s >*< x2s >*< x3s
          `shouldBe` [49, 59, 69]

    it "test 3" $ do
      (\a b c d -> 2 * a + 3 * b + 5 * c -4 * d)
        >$< x1s >*< x2s >*< x3s >*< x4s
          `shouldBe` [9, 15, 21]

  describe "dividelist" $ do
    it "test 1" $ do
      divideList [3, 4, 5] `shouldBe` 3.75

  describe "dividelist'" $ do
    it "test 1" $ do
      divideList' [3, 4, 5] `shouldBe` ("<-3.0/<-4.0/<-5.0/1.0", 3.75)

  describe "<**> and <*?>" $ do
    let test msg l r = it msg $ do
          l <**> r `shouldBe` l <*?> r

    let testFail msg l r = it msg $ do
          l <**> r `shouldNotBe` l <*?> r

    let testFn msg l r x = it msg $ do
          (l <**> r) x `shouldBe` (l <*?> r) x

    test "Maybe" (Just 5) (Just (+ 2))

    testFail "List" [1, 1] [id, (+ 1)]

    test "ZipList" (ZipList [1, 1]) (ZipList [id, (+ 1)])

    testFail "Either" (Left "A") (Left "B" :: Either String (Int -> Int))

    testFail "Pair" ("A", 3) ("B", (+ 1))

    testFn "Partial function" length (const (+ 5)) [1, 2]
