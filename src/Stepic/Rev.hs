module Stepic.Rev where

import Data.List (unfoldr)

revRange :: (Char, Char) -> [Char]
revRange = unfoldr g
  where
    g (l, r)
      | l > r = Nothing
      | otherwise = Just (r, (l, pred r))