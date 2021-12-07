module FileRead where

import Text.Read

data DeserializeResult t = Failure String | Success (t, String) deriving (Show, Eq)

fromEither :: (Either String a) -> String -> DeserializeResult a
fromEither (Right value) remaining = Success $ (value, remaining)
fromEither (Left failure) _ = Failure failure

mapSuccess :: DeserializeResult a -> ((a, String) -> DeserializeResult b) -> DeserializeResult b
mapSuccess (Success result) mapF = mapF result
mapSuccess (Failure failure) _ = Failure failure

mapFailure :: DeserializeResult a -> (String -> String) -> DeserializeResult a
mapFailure (Failure failure) mapF = Failure $ mapF failure
mapFailure success _ = success

class StringDeserializer t where
  deserialize :: String -> DeserializeResult t

deserializeTuple :: (StringDeserializer a, StringDeserializer b) => String -> DeserializeResult (a, b)
deserializeTuple str =
  mapSuccess
    (deserialize str)
    (\(aValue, nextStr) ->
      mapSuccess
        (deserialize nextStr)
        (\(bValue, remStr) ->
          Success ((aValue, bValue), remStr)) )

instance (StringDeserializer a, StringDeserializer b) => StringDeserializer (a, b) where
  deserialize str = deserializeTuple str

instance StringDeserializer Int where
  deserialize str = readInt [] str where
    readInt :: String -> String -> DeserializeResult Int
    readInt [] (next : others)
      | next == ' ' || next == ',' || next == '\n' = readInt [] others -- parse initial whitespace
      | otherwise = readInt [next] others
    readInt chars [] = fromEither (readEither chars) []
    readInt chars (next : remaining)
      | next == ' ' || next == ',' || next == '\n' = fromEither (readEither chars) (next : remaining)
      | otherwise = readInt (chars ++ [next]) remaining
