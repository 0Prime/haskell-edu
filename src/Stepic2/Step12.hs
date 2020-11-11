module Stepic2.Step12 where

import Control.Applicative (ZipList (ZipList), getZipList, liftA2)

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = (<$>)

(>*<) :: [a -> b] -> [a] -> [b]
fs >*< xs = getZipList $ ZipList fs <*> ZipList xs

divideList :: Fractional a => [a] -> a
divideList = foldr (/) 1

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' =
  let log x = "<-" ++ show x ++ "/"
   in foldr (\x acc -> (/) <$> (log x, x) <*> acc) ("1.0", 1)

infixl 4 <**>

infixl 4 <*?>

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

banana = Just 5 <**> Nothing