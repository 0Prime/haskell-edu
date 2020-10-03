module Stepic.MaybeList where

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] (: [])

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (h : _) = Just h