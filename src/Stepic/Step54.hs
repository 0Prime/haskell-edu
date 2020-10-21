module Stepic.Step54 (Token (..), asToken, tokenize) where

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
