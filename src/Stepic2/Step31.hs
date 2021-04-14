module Stepic2.Step31 where

newtype Except e a = Except {runExcept :: Either e a}
  deriving (Eq, Show)

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f ex = Except $ case runExcept ex of
  Left e -> Left $ f e
  Right x -> Right x