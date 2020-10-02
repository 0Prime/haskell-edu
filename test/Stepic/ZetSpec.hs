module Stepic.ZetSpec (spec) where

import Stepic.Zet
import Test.Hspec
import Test.Hspec.Tables (byExample)

spec :: Spec
spec = parallel $ do
  byExample
    ("Z", "z2i")
    [ (zero, 0),
      (Z Plus [One], 1),
      (Z Plus [Zero, One], 2),
      (Z Plus [One, One], 3),
      (Z Plus [Zero, Zero, One], 4)
    ]
    (\z e -> z2i z `shouldBe` e)

  byExample
    ("Int", "i2z")
    [ (0, zero),
      (1, Z Plus [One])
    ]
    (\i e -> i2z i `shouldBe` e)

  byExample
    ("Int", "int -> z -> int")
    [ (0, 0),
      (1, 0),
      (5, 0),
      (99, 0),
      ((-1), 0),
      ((-2), 0),
      ((-3), 0),
      ((-42), 0)
    ]
    (\n _ -> (z2i . i2z) n `shouldBe` n)

  byExample
    ("x", "y", "sum")
    [ (0, 0, 0),
      (0, 1, 1),
      (2, 40, 42),
      (0, (-1), (-1)),
      (1, (-1), (0)),
      ((-5), 4, (-1))
    ]
    (\x y s -> z2i (i2z x `add` i2z y) `shouldBe` s)

  byExample
    ("x", "y", "mul")
    [ (0, 0, 0),
      (0, 1, 0),
      (42, 1, 42),
      ((-1), 1, (-1)),
      ((-1), 0, (0)),
      (5, 10, 50)
    ]
    (\x y s -> z2i (i2z x `mul` i2z y) `shouldBe` s)
