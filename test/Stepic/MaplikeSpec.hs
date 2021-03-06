module Stepic.MaplikeSpec (spec) where

import Data.Function ((&))
import Stepic.Maplike
import Test.Hspec
import Prelude hiding (lookup)

spec :: Spec
spec = parallel $ do
  describe "MapLike ListMap" $ do
    let emptyListMap = empty :: ListMap Int Bool

    describe "empty" $ do
      it "constructs empty object" $ do
        emptyListMap `shouldBe` ListMap []

    describe "lookup" $ do
      it "Nothing on empty ListMap" $ do
        lookup 1 emptyListMap `shouldBe` Nothing

      it "Nothing when where are no requested key present" $ do
        lookup 2 (ListMap [(1, "a")]) `shouldBe` Nothing

      let testLookup k xs e =
            it (show k ++ " on " ++ show xs ++ " is Just " ++ show e) $ do
              lookup k (ListMap xs) `shouldBe` Just e

      testLookup 1 [(1, "a")] "a"
      testLookup 1 [(1, "a"), (2, "b")] "a"
      testLookup 2 [(1, "a"), (2, "b")] "b"

    describe "delete" $ do
      it "empty on empty ListMap" $ do
        delete 1 emptyListMap `shouldBe` emptyListMap

      it "empty on matching key and singleton ListMap" $ do
        delete 1 (ListMap [(1, "a")]) `shouldBe` empty

      context "sigleton on key match and size 2 ListMap" $ do
        let test key xs expected = it "" $ do
              delete key (ListMap xs) `shouldBe` ListMap expected

        test 1 [(1, "a"), (2, "b")] [(2, "b")]
        test 2 [(1, "a"), (2, "b")] [(1, "a")]

    describe "insert" $ do
      it "singleton on empty ListMap" $ do
        insert 1 "a" empty `shouldBe` ListMap [(1, "a")]

      it "replaces value on key match" $ do
        insert 1 "b" (ListMap [(1, "a")]) `shouldBe` ListMap [(1, "b")]

  describe "MapLike ArrwoMap" $ do
    let nil = empty :: ArrowMap Int String
    let get k xs = lookup k xs
    let set k v xs = insert k v xs
    let del k xs = delete k xs

    it "lookup on empty is Nothing" $ do
      nil & get 1 & (`shouldBe` Nothing)

    it "insert k v & lookup k is Just v" $ do
      nil & set 1 "a" & get 1 & (`shouldBe` Just "a")

    it "empty & delete k & lookup k is Nothing" $ do
      nil & del 1 & get 1 & (`shouldBe` Nothing)

    it "set k v & del k & get k is Nothing" $ do
      nil & set 1 "a" & del 1 & get 1 & (`shouldBe` Nothing)

    it "set a1,b2 & del 1,2 is Nothing" $ do
      nil & set 1 "a" & set 2 "b" & del 1 & del 2 & get 1 & (`shouldBe` Nothing)

    it "set 1a & del 2 & get 1 is Just a" $ do
      nil & set 1 "a" & del 2 & get 1 & (`shouldBe` Just "a")

    it "set 1a & del 2 & get 2 is Just a" $ do
      nil & set 1 "a" & del 2 & get 2 & (`shouldBe` Nothing)

    it "get 3 on 1a,2b is Nothing" $ do
      nil & set 1 "a" & set 2 "b" & get 3 & (`shouldBe` Nothing)

    it "get 2 on 1a,2b is Just b" $ do
      nil & set 1 "a" & set 2 "b" & get 2 & (`shouldBe` Just "b")

    it "get 1 on 1a,2b is Just a" $ do
      nil & set 1 "a" & set 2 "b" & get 1 & (`shouldBe` Just "a")

    it "set 1a,1b & get 1 is Just b" $ do
      nil & set 1 "a" & set 1 "b" & get 1 & (`shouldBe` Just "b")