module Stepic2.Step12 where

import Control.Applicative (ZipList (ZipList), getZipList)

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = (<$>)

(>*<) :: [a -> b] -> [a] -> [b]
fs >*< xs = getZipList $ ZipList fs <*> ZipList xs