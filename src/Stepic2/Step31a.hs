module Stepic2.Step31a where

import Control.Monad.Trans.Except
import Data.Foldable

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) _ i | i < 0 = throwE ErrNegativeIndex
(!!!) xs i = maybe err return val
  where
    err = throwE $ ErrIndexTooLarge i
    val = fst <$> find ((i ==) . snd) (zip xs [0 .. i])