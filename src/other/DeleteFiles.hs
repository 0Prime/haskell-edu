module Main where

import Control.Monad (forM_)
import qualified Data.List as L
import System.Directory (getDirectoryContents, removeFile)

main :: IO ()
main = do
  putStr "Substring: "
  namePart <- getLine
  if null namePart
    then putStrLn "Canceled"
    else do
      ns <- getDirectoryContents "."
      forM_
        (filter (L.isInfixOf namePart) ns)
        (\n -> putStrLn ("Removing file: " ++ n) >> removeFile n)
