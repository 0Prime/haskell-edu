module Stepic2.Step31a where

import Control.Monad
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

data SumError = SumError Int ReadError
  deriving (Eq, Show)

trySum :: [String] -> Except SumError Integer
trySum xs = sum <$> zipWithM fn xs [1 ..]
  where
    fn s i = withExcept (SumError i) (tryRead s)

newtype SimpleError = Simple {getSimple :: String}
  deriving (Eq, Show)

instance Semigroup SimpleError where
  (<>) (Simple s1) (Simple s2) = Simple $ s1 ++ s2

instance Monoid SimpleError where
  mempty = Simple ""

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) =
  Simple $
    "[index (" ++ show i ++ ") is too large]"
