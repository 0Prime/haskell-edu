module Stepic.Coins where

import Data.Functor
import Data.List (partition)

insert :: [a] -> [a] -> [[a]]
insert values target = values <&> (\v -> v : target)

shrink :: Int -> [[Int]] -> [[Int]]
shrink n = filter (\xs -> sum xs <= n)

split :: Int -> [[Int]] -> ([[Int]], [[Int]])
split n xss = partition (\xs -> sum xs == n) xss