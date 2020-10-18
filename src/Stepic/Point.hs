module Stepic.Point where

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = distanceToOrigin $ Point (x1 - x2) (y1 - y2)

data Point3D a = Point3D a a a deriving (Show)

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point' (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
  fmap f (Point' a) = Point' (fmap f a)
  fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)