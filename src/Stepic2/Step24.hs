module Stepic2.Step24 where

import Stepic2.Step14

instance Monad PrsE where
  (>>=) (PrsE f) k = PrsE $ \s -> do
    (a, s') <- f s
    runPrsE (k a) s'