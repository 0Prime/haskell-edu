module Stepic.Coins where

import Data.List (partition)

insert :: [a] -> [a] -> [[a]]
insert values target = map (\v -> v : target) values

shrink :: Int -> [[Int]] -> [[Int]]
shrink n = filter (\xs -> sum xs <= n)

split :: Int -> [[Int]] -> ([[Int]], [[Int]])
split n xss = partition (\xs -> sum xs == n) xss

change :: [Int] -> Int -> [[Int]]
change coins n = generate $ splitter $ nextGen []
  where
    generate :: ([[Int]], [[Int]]) -> [[Int]]
    generate (xss, []) = xss
    generate (xss, yss) = xss ++ (generate $ splitter $ yss >>= nextGen)
    nextGen = (shrink n) . inserter
    inserter = insert coins
    splitter = split n
