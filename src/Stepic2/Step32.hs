module Stepic2.Step32 where

decode c = c []

as ns c = c ns

a ns c = c ns

number :: [Int] -> Int
number = foldr1 $
  \l r -> if l > r then l * r else l + r

one = make 1

two = make 2

three = make 3

seventeen = make 17

twenty = make 20

hundred = make 100

thousand = make 1000

make :: Int -> [Int] -> ([Int] -> t) -> t
make n ns c = c (n : ns)