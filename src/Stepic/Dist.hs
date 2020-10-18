module Stepic.Dist where

import Lib

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = len [x1 - x2, y1 - y2]
