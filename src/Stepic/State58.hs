module Stepic.State58 where

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (MonadState (state), State, execState, modify, replicateM_)
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