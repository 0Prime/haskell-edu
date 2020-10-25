module Stepic.State58 where

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (MonadState (state), State)
import Control.Monad.Writer (Writer, runWriter)

readerToState :: Reader r a -> State r a
readerToState m = state $ \r -> (runReader m r, r)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m =
  let (a, w) = runWriter m
   in state $ \w' -> (a, w <> w')