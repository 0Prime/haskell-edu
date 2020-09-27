module Stepic.CoinsSpec (spec) where

import Data.Set (fromList)
import Stepic.Coins
import Test.Hspec
import Test.Hspec.Tables (byExample)

spec :: Spec
spec = parallel $ do
  describe "insert" $ do
    byExample
      ("values", "target", "expected")
      [ ("a", "", ["a"]),
        ("a", "a", ["aa"]),
        ("ab", "", ["a", "b"]),
        ("ab", "c", ["ac", "bc"])
      ]
      (\v t e -> insert v t `shouldBe` e)

  describe "shrink" $ do
    byExample
      ("threshold", "values", "expected")
      [ (1, [], []),
        (1, [[1]], [[1]]),
        (1, [[1], [2]], [[1]]),
        (2, [[1, 1], [1, 2]], [[1, 1]])
      ]
      (\t vs e -> shrink t vs `shouldBe` e)

  describe "split" $ do
    byExample
      ("threshold", "values", "expected")
      [ (3, [[1]], ([], [[1]])),
        (3, [[5], [3], [2]], ([[3]], [[5], [2]])),
        (5, [[5], [2, 3], [3, 2]], ([[5], [2, 3], [3, 2]], []))
      ]
      (\p vs e -> split p vs `shouldBe` e)

  tableTest "change0" change0
  tableTest "change1" change1
  tableTest "change2" change2
  tableTest "change3" change3

tableTest text fn =
  describe text $ do
    makeTest fn

makeTest f =
  byExample
    ("coins", "sum", "expected")
    [ ([1], 1, [[1]]),
      ([2], 1, []),
      ([1], 2, [[1, 1]]),
      ([2, 3], 5, [[3, 2], [2, 3]]),
      ([2, 3, 5, 7], 5, [[5], [3, 2], [2, 3]]),
      ([2, 3, 7], 7, [[7], [3, 2, 2], [2, 3, 2], [2, 2, 3]])
    ]
    (\c s e -> fromList (f c s) `shouldBe` (fromList e))
