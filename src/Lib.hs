module Lib
  ( len,
    digits,
    fibs,
    fibN,
    perms,
  )
where

import Data.List.Split (divvy)

len :: Floating a => [a] -> a
len = sqrt . sum . map (^ 2)

manhLen :: [Int] -> Int
manhLen = sum . map abs

digits :: Integral a => a -> a -> [a]
digits b x
  | x == 0 = [0]
  | x < 0 = digits b $ abs x
  | otherwise = digs x
  where
    digs 0 = []
    digs x = digs (x `div` b) ++ [x `mod` b]

fibs :: [Integer]
fibs = 0 : 1 : do rest
  where
    rest = map next1 $ divvy 2 1 fibs
    next1 [a, b] = a + b

fibN :: Int -> Integer
fibN = (fibs !!)

perms :: [a] -> [[a]]
perms = foldr (concatMap . insertions) [[]]
  where
    insertions x [] = [[x]]
    insertions x list@(y : ys) = (x : list) : map (y :) (insertions x ys)
