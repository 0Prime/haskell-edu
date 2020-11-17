module Stepic2.Step13 where

import Text.Parsec

getList :: Parsec String u [String]
getList = many1 digit `sepBy` char ';'

ignoreBraces ::
  Parsec String u a ->
  Parsec String u b ->
  Parsec String u c ->
  Parsec String u c
ignoreBraces lBrace rBrace body = lBrace *> body <* rBrace