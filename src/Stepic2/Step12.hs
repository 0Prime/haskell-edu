module Stepic2.Step12 where

import Control.Applicative (ZipList (ZipList), getZipList)

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = (<$>)

(>*<) :: [a -> b] -> [a] -> [b]
fs >*< xs = getZipList $ ZipList fs <*> ZipList xs

divideList :: Fractional a => [a] -> a
divideList = foldr (/) 1

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' =
  let log x = "<-" ++ show x ++ "/"
   in foldr (\x acc -> (/) <$> (log x, x) <*> acc) ("1.0", 1)