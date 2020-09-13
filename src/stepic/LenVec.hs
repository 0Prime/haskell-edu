len :: Floating a => [a] -> a
len = sqrt . sum . map (^ 2)

lenVec3 :: Floating a => a -> a -> a -> a
lenVec3 x y z = len [x, y, z]

ret = lenVec3 3 4 5

main :: IO ()
main = putStrLn . show $ ret