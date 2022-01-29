module Parsing where

import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Lib

numParser :: Parser Int
numParser = do
  digits <- many1 digit
  return (read digits)

coordinateParser :: Parser Coordinate
coordinateParser = do
  xVal <- numParser
  void (char ',')
  Coordinate xVal <$> numParser

ventLineParser :: Parser VentLine
ventLineParser = do
  coordA <- coordinateParser
  void (string " -> ")
  VentLine coordA <$> coordinateParser

ventLinesParser :: Parser [VentLine]
ventLinesParser = many (ventLineParser <* newline)
