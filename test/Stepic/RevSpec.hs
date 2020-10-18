module Stepic.RevSpec (spec) where

import Stepic.Rev
import Test.Hspec
import Test.Hspec.Tables (byExample)

spec :: Spec
spec = parallel $ do
  byExample
    ("range", "expected")
    [ (('a', 'a'), "a"),
      (('z', 'z'), "z"),
      (('a', 'z'), "zyxwvutsrqponmlkjihgfedcba")
    ]
    (\r e -> revRange r `shouldBe` e)