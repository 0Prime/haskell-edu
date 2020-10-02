module Stepic.Zet where

import Lib

data Bit = Zero | One deriving (Show, Eq)

data Sign = Minus | Plus deriving (Show, Eq)

data Z = Z Sign [Bit] deriving (Show, Eq)

add :: Z -> Z -> Z
add a b = integer2z ((z2integer a) + (z2integer b))

mul :: Z -> Z -> Z
mul a b = integer2z ((z2integer a) * (z2integer b))

bit2num :: Num p => Bit -> p
bit2num Zero = 0
bit2num One = 1

num2bit :: (Eq a, Num a) => a -> Bit
num2bit 0 = Zero
num2bit 1 = One

sign :: Num p => Sign -> p
sign Minus = -1
sign Plus = 1

toSignum n = if signum n >= 0 then Plus else Minus

z2integer :: Z -> Integer
z2integer (Z s xs) = (sign s) * (sum $ zipWith (*) (map bit2num xs) (map (2 ^) [0 ..]))

integer2z :: Integer -> Z
integer2z n = Z (toSignum n) $ reverse $ (dropWhile (== Zero)) $ map num2bit (digits 2 (abs n))

zero = Z Plus []