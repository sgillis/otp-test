module Config (parse) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import System.Exit

parse :: String -> IO [(String, Int)]
parse filename =
  parseFromFile parser filename >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure

parser :: Parser [(String, Int)]
parser =
  manyTill parseLine eof

parseLine :: Parser (String, Int)
parseLine = do
  port <- many1 digit
  char ','
  seed <- many1 digit
  optional $ char '\n'
  return (port, read seed)
