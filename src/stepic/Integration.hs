integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b
  | a - b == 0 = 0
  | otherwise = h * ((f a + f b) / 2 + sum (map f xs))
  where
    h = (b - a) / n
    n = 1000
    xs = [a + h, a + 2 * h .. b - h]