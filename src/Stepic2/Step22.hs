module Stepic2.Step22 where

import Control.Applicative ((<*>))
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
