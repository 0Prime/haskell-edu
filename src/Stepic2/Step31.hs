module Stepic2.Step31 where

newtype Except e a = Except {runExcept :: Either e a} deriving (Show)

except = Except

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = undefined