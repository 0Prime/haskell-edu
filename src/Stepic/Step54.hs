module Stepic.Step54 where

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