module Lib
    ( runParser,
      jsonValue
    ) where

import Data.Char
import Control.Applicative

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Double
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = 
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

jsonValue :: Parser JsonValue
jsonValue = ws *> (jsonNull <|> jsonBool <|> jsonString <|> jsonNumber <|> jsonArray <|> jsonObject) <* ws

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    mkJsonBool str b = JsonBool b <$ stringP str
    jsonTrue = mkJsonBool "true" True
    jsonFalse = mkJsonBool "false" False

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
    where elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject
            <$> (charP '{'
            *> ws *>
            sepBy ( ws *> charP ',' <* ws) pair
            <* ws <*
            charP '}')
    where pair = (\key _ value -> (key, value))
                              <$> stringLiteral
                              <*> (ws *> charP ':' <* ws)
                              <*> jsonValue

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> doubleLiteral

doubleLiteral :: Parser Double
doubleLiteral =
  doubleFromParts
    <$> (minus <|> pure 1)
    <*> (read <$> digits)
    <*> ((read <$> (('0':) <$> ((:) <$> charP '.' <*> digits))) <|> pure 0)
    <*> ((e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits))) <|> pure 0)
  where
    digits = notNull (spanP isDigit)
    minus = (-1) <$ charP '-'
    plus = 1 <$ charP '+'
    e = charP 'e' <|> charP 'E'

doubleFromParts :: Integer  -- sign
                -> Integer  -- integral part
                -> Double   -- decimal part
                -> Integer  -- exponent
                -> Double
doubleFromParts sign int dec expo =
  fromIntegral sign * (fromIntegral int + dec) * (10 ^^ expo)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = ((:) <$> element <*> many (sep *> element)) <|> pure []

ws :: Parser String
ws = spanP isSpace

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
                        (input', xs) <- p input
                        if null xs
                            then Nothing
                            else Just (input', xs)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
                        let (token, rest) = span f input
                            in Just (rest, token)

charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
            | y == x    = Just (ys, x)
            | otherwise = Nothing
        f [] =  Nothing

stringP :: String -> Parser String
stringP = traverse charP

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'