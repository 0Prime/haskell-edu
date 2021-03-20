module Stepic2.Step24 where

import Stepic2.Step14
import Stepic2.Step23

instance Monad PrsE where
  (>>=) (PrsE f) k = PrsE $ \s -> do
    (a, s') <- f s
    runPrsE (k a) s'

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) cs = Bi a b cs
concat3OC (Un a) (Bi b1 b2 bs) cs = Bi a b1 $ concat3OC (Un b2) bs cs
concat3OC (Bi a1 a2 as) bs cs = Bi a1 a2 $ concat3OC as bs cs

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi a b xs) = concat3OC a b $ concatOC xs