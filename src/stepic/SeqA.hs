import Data.List.Split (divvy)

seqA = (seq !!)
  where
    seq = 1 : 2 : 3 : do rest
    next1 [a, b, c] = c + b - 2 * a
    rest = map next1 $ divvy 3 1 seq