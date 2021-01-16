module Stepic2.Step15 where

newtype Cmps3 f g h a = Cmps3 {getCmps3 :: f (g (h a))}
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap fun (Cmps3 x) = Cmps3 $ (fmap . fmap . fmap) fun x
