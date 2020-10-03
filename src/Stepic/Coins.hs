module Stepic.Coins where

import Data.List (partition)

insert :: [a] -> [a] -> [[a]]
insert values target = map (: target) values

shrink :: (Ord a, Num a) => a -> [[a]] -> [[a]]
shrink n = filter (\xs -> sum xs <= n)

split :: (Eq a, Num a) => a -> [[a]] -> ([[a]], [[a]])
split n = partition (\xs -> sum xs == n)

change0 :: (Ord a, Num a) => [a] -> a -> [[a]]
change0 cs n = generate $ splitter $ nextGen []
  where
    generate (xss, []) = xss
    generate (xss, yss) = xss ++ generate (splitter $ yss >>= nextGen)
    nextGen = shrink n . insert cs
    splitter = split n

changeGuard :: (Ord a, Num a) => ([a] -> a -> [[a]]) -> [a] -> a -> [[a]]
changeGuard _ _ 0 = [[]]
changeGuard _ _ n | n < 0 = []
changeGuard f coins n = f coins n

change1 :: (Ord a, Num a) => [a] -> a -> [[a]]
change1 = changeGuard $ \coins n ->
  [coin : rest | coin <- coins, rest <- change1 coins (n - coin)]

change2 :: (Ord a, Num a) => [a] -> a -> [[a]]
change2 = changeGuard $ \coins n ->
  do coin <- coins; rest <- change2 coins (n - coin); [coin : rest]

change3 :: (Ord a, Num a) => [a] -> a -> [[a]]
change3 = changeGuard $ \coins n ->
  coins >>= \coin -> (coin :) <$> change3 coins (n - coin)

--

change :: (Ord a, Num a) => a -> [[a]]
change = change3 coins

coins :: (Num a) => [a]
coins = [2, 3, 4]