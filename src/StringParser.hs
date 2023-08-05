module StringParser (module StringParser) where

import Control.Applicative (Alternative (..), (<|>))

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (res, rem_s) <- p s
    return (f res, rem_s)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)

  (Parser f) <*> (Parser p) = Parser $ \s -> do
    (f', s1) <- f s
    (res, s2) <- p s1
    return (f' res, s2)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> do
    (res, s1) <- p s
    runParser (f res) s1

instance Alternative Parser where
  empty = Parser $ const Nothing

  (Parser x) <|> (Parser y) = Parser $ \s ->
    case x s of
      Just res -> Just res
      Nothing -> y s

char :: Char -> Parser Char
char c = Parser matchChar
  where
    matchChar [] = Nothing
    matchChar (x : xs)
      | x == c = Just (x, xs)
      | otherwise = Nothing

string :: String -> Parser String
string = mapM char

charIf :: (Char -> Bool) -> Parser Char
charIf f = Parser matchChar
  where
    matchChar [] = Nothing
    matchChar (x : xs)
      | f x = Just (x, xs)
      | otherwise = Nothing
