module FileIO where

  readText :: FilePath -> IO [String]
  readText filePath = do
    contents <- readFile filePath
    return $ lines contents

  class TextParsable t where
    fromText :: String -> t

  parseLines :: TextParsable t => FilePath -> IO [t]
  parseLines filePath = do
    textLines <- readText filePath
    return $ map fromText textLines
    