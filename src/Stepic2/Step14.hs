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

newtype PrsE a = PrsE {runPrsE :: String -> Either String (a, String)}

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE f
  where
    f "" = Left "unexpected end of input"
    f (h : t)
      | p h = Right (h, t)
      | otherwise = Left ("unexpected " ++ [h])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)