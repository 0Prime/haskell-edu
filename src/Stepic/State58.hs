{-# LANGUAGE DeriveTraversable #-}

module Stepic.State58 where

import Control.Monad.Identity
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State
import Control.Monad.Writer (Writer, runWriter)

readerToState :: Reader r a -> State r a
readerToState m = state $ \r -> (runReader m r, r)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m =
  let (a, w) = runWriter m
   in state $ \w' -> (a, w <> w')

fibStep :: State (Integer, Integer) ()
fibStep = modify $ \(a, b) -> (b, a + b)

execStateN :: Int -> State s a -> s -> s
execStateN n = execState . replicateM_ n

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (traverse tick tree) 1

tick :: a -> State Integer Integer
tick _ = do
  n <- get
  put $ succ n
  return n