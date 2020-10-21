module Stepic.Step54
  ( Token (..),
    asToken,
    tokenize,
    pythagoreanTriple,
  )
where

import Control.Monad (guard)
import Text.Read (readMaybe)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken s = case s of
  "+" -> Just Plus
  "-" -> Just Minus
  "(" -> Just LeftBrace
  ")" -> Just RightBrace
  _ -> fmap Number (readMaybe s)

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words

data Board = Board

nextPositions :: Board -> [Board]
nextPositions = undefined

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN board turns pred =
  filter pred $ iterate (>>= nextPositions) [board] !! turns

-- a2+b2=c2,a>0,b>0,c>0,c≤x,a<b

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
  | x <= 0 = []
  | otherwise = do
    let xs = [1 .. x]
    a <- xs
    b <- xs
    c <- xs
    guard $ a ^ 2 + b ^ 2 == c ^ 2
    guard $ a < b
    return (a, b, c)