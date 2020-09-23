module Stepic.CoinsSpec (spec) where

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