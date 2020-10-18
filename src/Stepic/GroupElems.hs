module Stepic.GroupElems where

groupElems :: Eq a => [a] -> [[a]]
groupElems = foldl f []
  where
    f [] x = [[x]]
    f acc@(ms : mss) x = if x `elem` ms then (x : ms) : mss else [x] : acc