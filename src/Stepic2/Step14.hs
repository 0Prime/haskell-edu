{-# LANGUAGE DeriveFunctor #-}

module Stepic2.Step14 where

import Data.List

newtype Prs a = Prs {runPrs :: String -> Maybe (a, String)}
  deriving (Functor)

instance Applicative Prs where
  pure x = Prs $ \s -> Just (x, s)

  (<*>) (Prs fl) (Prs fr) = Prs $ \s -> do
    (f', s') <- fl s
    (a, s'') <- fr s'
    return (f' a, s'')

anyChr :: Prs Char
anyChr = Prs uncons