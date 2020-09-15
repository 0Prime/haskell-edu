module Lib
  ( len,
    digits,
  )
where

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