module Stepic.Coins where

import Data.List (partition)

insert :: [a] -> [a] -> [[a]]
insert values target = map (\v -> v : target) values

shrink :: (Ord a, Num a) => a -> [[a]] -> [[a]]
shrink n = filter (\xs -> sum xs <= n)

split :: (Eq a, Num a) => a -> [[a]] -> ([[a]], [[a]])
split n xss = partition (\xs -> sum xs == n) xss

change' :: (Ord a, Num a) => [a] -> a -> [[a]]
change' cs n = generate $ splitter $ nextGen []
  where
    generate (xss, []) = xss
    generate (xss, yss) = xss ++ (generate $ splitter $ yss >>= nextGen)
    nextGen = (shrink n) . (insert cs)
    splitter = split n

change :: (Ord a, Num a) => a -> [[a]]
change n = change' coins n

coins :: (Num a) => [a]
coins = [2, 3, 4]