{-# LANGUAGE DeriveFunctor #-}

module Stepic2.Step14 where

import Data.List

newtype Prs a = Prs {runPrs :: String -> Maybe (a, String)}
  deriving (Functor)

anyChr :: Prs Char
anyChr = Prs uncons