module Stepic.MaplikeSpec (spec) where

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