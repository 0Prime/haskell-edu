module Stepic.Coins where

import Data.Functor

insert :: [a] -> [a] -> [[a]]
insert values target = values <&> (\v -> v : target)
