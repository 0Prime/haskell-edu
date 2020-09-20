-- inspired by https://themonadreader.files.wordpress.com/2014/04/fizzbuzz.pdf

fizzbuzz :: Int -> String
fizzbuzz n = (test 3 "fizz" . test 5 "buzz") id (show n)
  where
    test d s x
      | n `mod` d == 0 = const (s ++ x "")
      | otherwise = x
