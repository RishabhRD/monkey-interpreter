module LibParse (module LibParse) where

import Control.Applicative (Alternative (empty), (<|>))

newtype Parser t a = Parser {runParser :: [t] -> Maybe (a, [t])}

instance Functor (Parser t) where
  fmap f (Parser p) = Parser $ \s -> do
    (res, rem_s) <- p s
    return (f res, rem_s)

instance Applicative (Parser t) where
  pure x = Parser $ \s -> Just (x, s)

  (Parser f) <*> (Parser p) = Parser $ \s -> do
    (f', s1) <- f s
    (res, s2) <- p s1
    return (f' res, s2)

instance Monad (Parser t) where
  (Parser p) >>= f = Parser $ \s -> do
    (res, s1) <- p s
    runParser (f res) s1

instance Alternative (Parser t) where
  empty = Parser $ const Nothing

  (Parser x) <|> (Parser y) = Parser $ \s ->
    case x s of
      Just res -> Just res
      Nothing -> y s

symIf :: (t -> Bool) -> Parser t t
symIf f = Parser matchSym
  where
    matchSym [] = Nothing
    matchSym (x : xs)
      | f x = Just (x, xs)
      | otherwise = Nothing

lst :: (Eq t) => [t] -> Parser t [t]
lst = mapM sym

sym :: (Eq t) => t -> Parser t t
sym c = symIf (== c)

type StringParser = Parser Char
