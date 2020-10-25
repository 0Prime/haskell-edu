module Stepic.State58 where

import Control.Monad.Reader --(Reader)
import Control.Monad.State --(State)

readerToState :: Reader r a -> State r a
readerToState m = state (\r -> (runReader m r, r))
