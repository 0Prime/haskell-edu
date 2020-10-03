module Stepic.Coord where

data Coord a = Coord a a deriving (Eq, Show)

instance Functor Coord where
  fmap f (Coord a b) = Coord (f a) (f b)

getCenter :: Double -> Coord Int -> Coord Double
getCenter size = fmap $ (\n -> size * n + size / 2) . fromIntegral

getCell :: Double -> Coord Double -> Coord Int
getCell size = fmap (\n -> floor (n / size))