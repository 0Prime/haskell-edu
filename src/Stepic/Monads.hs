module Stepic.Monads where

data Log a = Log [String] a

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a f1 f2 = Log (msg1 ++ msg2) c
  where
    Log msg1 b = f1 a
    Log msg2 c = f2 b

returnLog :: a -> Log a
returnLog = Log []