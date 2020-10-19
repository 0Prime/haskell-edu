module Stepic.Functors where

data Point3D a = Point3D a a a deriving (Show)

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point' (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
  fmap f (Point' a) = Point' (fmap f a)
  fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f <$> a)
  fmap f (Branch l a r) = Branch (f <$> l) (f <$> a) (f <$> r)

data Entry k1 k2 v = Entry (k1, k2) v deriving (Eq, Show)

newtype Map k1 k2 v = Map [Entry k1 k2 v] deriving (Eq, Show)

instance Functor (Entry k1 k2) where
  fmap f (Entry k v) = Entry k (f v)

instance Functor (Map k1 k2) where
  fmap f (Map xs) = Map $ fmap (fmap f) xs