module Lib
  ( len,
    digits,
    fibN,
  )
where

import Data.List.Split

len :: Floating a => [a] -> a
len = sqrt . sum . map (^ 2)

digits :: Integral a => a -> [a]
digits x
  | x == 0 = [0]
  | x < 0 = digits $ abs x
  | otherwise = digs x
  where
    digs 0 = []
    digs x = digs (x `div` 10) ++ [x `mod` 10]

fibN = (fibs !!)
  where
    fibs = 0 : 1 : do rest
    rest = map next1 $ divvy 2 1 fibs
    next1 [a, b] = a + b