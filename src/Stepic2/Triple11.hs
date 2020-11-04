module Stepic2.Triple11 where

data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)