{-# LANGUAGE TypeOperators #-}

module Stepic2.Step21 where

import Data.Monoid
import Stepic2.Step15

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)

newtype Postorder a = PostO (Tree a) deriving (Eq, Show)

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldr f ini (Branch l a r) =
    g f l $ f a $ g f r ini
  foldr _ ini _ = ini

instance Foldable Preorder where
  foldr f ini (PreO (Branch l a r)) =
    f a $ g f (PreO l) $ g f (PreO r) ini
  foldr _ ini _ = ini

instance Foldable Postorder where
  foldr f ini (PostO (Branch l a r)) =
    g f (PostO l) $ g f (PostO r) $ f a ini
  foldr _ ini _ = ini

g :: Foldable t => (a -> b -> b) -> t a -> b -> b
g = flip . foldr

instance Foldable Levelorder where
  foldr f ini (LevelO tree) =
    foldr f ini $ flatten [tree]

flatten :: [Tree a] -> [a]
flatten [] = []
flatten (Nil : xs) = flatten xs
flatten ((Branch l a r) : xs) = a : flatten (xs ++ [l, r])

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldMap f (Cmps x) = (foldMap . foldMap) f x
