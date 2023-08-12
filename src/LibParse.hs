module LibParse (module LibParse) where

import Control.Applicative (Alternative (empty), (<|>))

newtype Parser t e a = Parser {runParser :: [t] -> Either [e] (a, [t])}

instance Functor (Parser t e) where
  fmap f (Parser p) = Parser $ \s -> do
    (res, rem_s) <- p s
    return (f res, rem_s)

instance Applicative (Parser t e) where
  pure x = Parser $ \s -> Right (x, s)

  (Parser f) <*> (Parser p) = Parser $ \s -> do
    (f', s1) <- f s
    (res, s2) <- p s1
    return (f' res, s2)

instance Monad (Parser t e) where
  (Parser p) >>= f = Parser $ \s -> do
    (res, s1) <- p s
    runParser (f res) s1

instance Alternative (Parser t e) where
  empty = Parser $ const $ Left []

  (Parser x) <|> (Parser y) = Parser $ \s ->
    case x s of
      Right res -> Right res
      Left errs -> case y s of
        Right res -> Right res
        Left errs' -> Left $ errs ++ errs'

symIf :: (t -> Bool) -> Parser t e t
symIf f = Parser matchSym
  where
    matchSym [] = Left []
    matchSym (x : xs)
      | f x = Right (x, xs)
      | otherwise = Left []

lst :: (Eq t) => [t] -> Parser t e [t]
lst = mapM sym

sym :: (Eq t) => t -> Parser t e t
sym c = symIf (== c)

type StringParser = Parser Char ()
