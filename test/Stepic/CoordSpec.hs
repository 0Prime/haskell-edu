module Stepic.CoordSpec (spec) where

import Stepic.Coord
import Test.HUnit.Approx (assertApproxEqual)
import Test.Hspec
import Test.Hspec.Tables (byExample)

spec :: Spec
spec = parallel $ do
  byExample
    ("size", "cell address", "cell center coord")
    [ (1, Coord 0 0, Coord 0.5 0.5),
      (1, Coord (-1) (-1), Coord (-0.5) (-0.5)),
      (2.5, Coord 1 2, Coord 3.75 6.25),
      (2.2, Coord 2 1, Coord 5.5 3.3),
      (2.2, Coord 2 (-1), Coord 5.5 (-1.1)),
      (8, Coord (-1) (-1), Coord (-4.0) (-4.0)),
      (5.0, Coord 2 3, Coord 12.5 17.5)
    ]
    (\d ca cc -> getCenter d ca ~= cc)

  byExample
    ("size", "coord", "cell address")
    [ (1, Coord 0.1 0.1, Coord 0 0),
      (1, Coord 1 1, Coord 1 1),
      (1, Coord 1.1 1.1, Coord 1 1),
      (1, Coord 0.5 0, Coord 0 0),
      (1, Coord 10 10, Coord 10 10),
      (1, Coord (-1) (-1), Coord (-1) (-1)),
      (5, Coord 15.5 (-42.9), Coord 3 (-9)),
      (2.2, Coord 3.2 1.6, Coord 1 0),
      (10, Coord 23 47, Coord 2 4)
    ]
    (\d ca ce -> getCell d ca `shouldBe` ce)

(~=) c1@(Coord x1 y1) c2@(Coord x2 y2) =
  x1 `f` x2 >> y1 `f` y2
  where
    f a b = assertApproxEqual msg 0.00001 b a
    msg = (show c1 ++ " ~= " ++ show c2)