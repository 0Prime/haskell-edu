{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype Temperature a = Temperature Double
  deriving (Num, Fractional, Show, Eq)

data Celsius

data Fahrenheit

data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature k) = Temperature k - 273.15