module Stepic2.Step25 where

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

charEP :: Char -> PrsEP Char
charEP c = satisfyEP (== c)

anyEP :: PrsEP Char
anyEP = satisfyEP (const True)

newtype PrsEP a = PrsEP
  { runPrsEP ::
      Int ->
      String ->
      (Int, Either String (a, String))
  }

instance Functor PrsEP where
  fmap fab (PrsEP fa) = PrsEP fun
    where
      fun n s = case fa n s of
        (n', Right (a, s')) -> (n', Right (fab a, s'))
        (n', Left e) -> (n', Left e)

instance Applicative PrsEP where
  pure a = PrsEP $ \n s -> (n, Right (a, s))

  (<*>) (PrsEP fab) (PrsEP fa) = PrsEP fun
    where
      fun n s = case fab n s of
        (n', Right (ab, s')) -> g ab (fa n' s')
        (n', Left e) -> (n', Left e)

      g f (n'', Right (y, s'')) = (n'', Right (f y, s''))
      g _ (n'', Left e) = (n'', Left e)