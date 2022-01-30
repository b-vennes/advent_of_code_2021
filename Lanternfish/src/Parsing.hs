module Parsing where

import Text.Parsec
    ( char, digit, many1, optional, many, parse, ParseError )
import Text.Parsec.String ( Parser )
import Lib (Lanternfish(Lanternfish))

numParser :: Parser Int
numParser = do
  digits <- many1 digit
  return (read digits)

parseLanternFish :: Parser Lanternfish
parseLanternFish = do
  int <- numParser
  _ <- optional (char ',')
  return (Lanternfish int)

parseLanternfishPool :: Parser [Lanternfish]
parseLanternfishPool = many parseLanternFish

parsingLanternfishProgram :: String -> Either ParseError [Lanternfish]
parsingLanternfishProgram = parse parseLanternfishPool ""
