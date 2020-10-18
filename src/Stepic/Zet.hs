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

import Data.Function ((&))
import Data.Functor ((<&>))
import Lib

data Bit = Zero | One deriving (Show, Eq)

data Sign = Minus | Plus deriving (Show, Eq)

data Z = Z Sign [Bit] deriving (Show, Eq)

add :: Z -> Z -> Z
add a b = z2i a + z2i b & i2z

mul :: Z -> Z -> Z
mul a b = z2i a * z2i b & i2z

bit2num :: Num n => Bit -> n
bit2num b = if b == Zero then 0 else 1

num2bit :: (Eq n, Num n) => n -> Bit
num2bit n = if n == 0 then Zero else One

toSignum :: Num n => Sign -> n
toSignum s = if s == Minus then (-1) else 1

toSign :: (Ord n, Num n) => n -> Sign
toSign n = if signum n >= 0 then Plus else Minus

z2i :: Z -> Integer
z2i (Z s xs) = toSignum s * (nums & zipWith (*) powersOftwo & sum)
  where
    nums = xs <&> bit2num
    powersOftwo = [0 ..] <&> (2 ^)

i2z :: Integer -> Z
i2z n = n & digits 2 & map num2bit & dropWhile (== Zero) & reverse & Z (toSign n)

zero = Z Plus []