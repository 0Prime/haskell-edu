module Stepic2.Step31a where

import Control.Monad.Trans.Except
import Data.Foldable
import Text.Read

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) _ i | i < 0 = throwE ErrNegativeIndex
(!!!) xs i = maybe err return val
  where
    err = throwE $ ErrIndexTooLarge i
    val = fst <$> find ((i ==) . snd) (zip xs [0 .. i])

data ReadError = EmptyInput | NoParse String
  deriving (Eq, Show)

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = maybe err return val
  where
    err = throwE $ NoParse s
    val = readMaybe s