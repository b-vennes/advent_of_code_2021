module Parsing
  (
    parseNumber
  , parseCrab
  , parseCrabs
  , parseFile
  ) where

import Text.Parsec (parse, digit, many1, optional, char, ParseError)
import Text.Parsec.String (Parser)
import Lib (Crabs(Crabs), Crab(Crab))

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseCrab :: Parser Crab
parseCrab = do
  num <- parseNumber
  _ <- optional $ char ','
  return $ Crab num

parseCrabs :: Parser Crabs
parseCrabs = Crabs <$> many1 parseCrab

parseFile :: FilePath -> Parser a -> IO (Either ParseError a)
parseFile file parser = do
  fileContents <- readFile file
  let parsed = parse parser "" fileContents
  return parsed