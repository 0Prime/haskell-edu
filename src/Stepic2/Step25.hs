module Stepic2.Step25 where

newtype PrsEP a = PrsEP {runPrsEP :: Int -> String -> (Int, Either String (a, String))}

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP p = PrsEP $ f . succ
  where
    f n "" = (n, Left $ err n "end of input")
    f n (h : t)
      | p h = (n, Right (h, t))
      | otherwise = (n, Left $ err n [h])

    err n msg = "pos " ++ show n ++ ": unexpected " ++ msg
