module Lib
  ( len,
  )
where

len :: Floating a => [a] -> a
len = sqrt . sum . map (^ 2)
