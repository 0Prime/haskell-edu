module Stepic.Functors where

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f <$> a)
  fmap f (Branch l a r) = Branch (f <$> l) (f <$> a) (f <$> r)