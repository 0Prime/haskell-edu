module Stepic.Zet
  ( Z (..),
    Sign (..),
    Bit (..),
    add,
    mul,
    z2i,
    i2z,
    zero,
  )
where

import Lib

data Bit = Zero | One deriving (Show, Eq)

data Sign = Minus | Plus deriving (Show, Eq)

data Z = Z Sign [Bit] deriving (Show, Eq)

add :: Z -> Z -> Z
add a b = i2z $ z2i a + z2i b

mul :: Z -> Z -> Z
mul a b = i2z $ z2i a * z2i b

bit2num :: Num p => Bit -> p
bit2num Zero = 0
bit2num One = 1

num2bit :: (Eq a, Num a) => a -> Bit
num2bit 0 = Zero
num2bit 1 = One

toSignum :: Num p => Sign -> p
toSignum Minus = -1
toSignum Plus = 1

toSign :: (Ord a, Num a) => a -> Sign
toSign n = if signum n >= 0 then Plus else Minus

z2i :: Z -> Integer
z2i (Z s xs) = toSignum s * (sum $ zipWith (*) nums powersOftwo)
  where
    nums = bit2num <$> xs
    powersOftwo = (2 ^) <$> [0 ..]

i2z :: Integer -> Z
i2z n = Z (toSign n) $ reverse $ dropWhile (== Zero) $ map num2bit $ digits 2 $ abs n

zero = Z Plus []