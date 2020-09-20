groupElems :: Eq a => [a] -> [[a]]
groupElems xs = foldl f [] xs
  where
    f [] x = [[x]]
    f acc@(ms : mss) x = if x `elem` ms then (x : ms) : mss else [x] : acc