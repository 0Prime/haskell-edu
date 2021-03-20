{-# LANGUAGE TypeOperators #-}

module Stepic2.Step22 where

import Control.Applicative (Applicative (liftA2), liftA, (<*>))
import Data.Functor.Compose
import Lib (Tree (..))
import Stepic2.Step15
import Stepic2.Step21
import Stepic2.Triple11

sequenceA2list :: (Foldable t, Applicative f) => t (f a) -> f [a]
sequenceA2list = foldr fn (pure [])
  where
    fn :: (Applicative f) => f b -> f [b] -> f [b]
    fn x y = (:) <$> x <*> y

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\x y -> (:) <$> f x <*> y) (pure [])
  where
    fn x y = (:) <$> f x <*> y

instance Traversable Triple where
  traverse f (Tr x y z) = Tr <$> f x <*> f y <*> f z

data Result a = Ok a | Error String deriving (Eq, Show)

instance Functor Result where
  fmap f (Ok a) = Ok $ f a
  fmap _ (Error s) = Error s

instance Foldable Result where
  foldr f b (Ok a) = f a b
  foldr _ b _ = b

instance Traversable Result where
  traverse f (Ok a) = Ok <$> f a
  traverse f (Error s) = pure $ Error s

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse g (Branch l a r) =
    Branch
      <$> traverse g l
      <*> g a
      <*> traverse g r

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse f (Cmps x) = Cmps <$> traverse (traverse f) x