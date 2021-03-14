module Stepic2.Step23 where

import Data.Traversable

data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

instance Traversable OddC where
  traverse f (Un a) = Un <$> f a
  traverse f (Bi a1 a2 x) =
    Bi <$> f a1 <*> f a2 <*> traverse f x

instance Foldable OddC where
  foldMap = foldMapDefault

instance Functor OddC where
  fmap = fmapDefault