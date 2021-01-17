{-# LANGUAGE TypeOperators #-}

module Stepic2.Step15 where

infixr 9 |.|

newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap fun (Cmps x) = Cmps $ (fmap . fmap) fun x

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap unCmps3 . getCmps

newtype Cmps3 f g h a = Cmps3 {getCmps3 :: f (g (h a))}
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap fun (Cmps3 x) = Cmps3 $ (fmap . fmap . fmap) fun x
