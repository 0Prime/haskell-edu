module Stepic.Person where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Eq, Show)

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Eq, Show)

parsePerson :: String -> Either Error Person
parsePerson "" = Left ParsingError
parsePerson txt = makePerson . Map.fromList =<< sequence (parsePair . splitOn " = " <$> lines txt)

parsePair [k, v] = Right (k, v)
parsePair _ = Left ParsingError

makePerson :: Map.Map String String -> Either Error Person
makePerson m = do
  firstName <- lookupEither "firstName" m >>= parseString
  lastName <- lookupEither "lastName" m >>= parseString
  age <- lookupEither "age" m >>= parseString >>= parseInt
  return $ Person firstName lastName age

lookupEither :: Ord k => k -> Map.Map k a -> Either Error a
lookupEither k m = maybeToRight IncompleteDataError $ Map.lookup k m

parseString x = if null x then Left ParsingError else Right x

parseInt str = maybeToRight (IncorrectDataError str) $ readMaybe str

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing = Left y