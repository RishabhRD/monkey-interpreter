module LibParse (module LibParse) where

import Control.Applicative (Alternative (empty), (<|>))
import Data.Bifunctor (Bifunctor (bimap))

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

instance Bifunctor (Parser t) where
  bimap f g (Parser p) = Parser $ \s -> do
    let res = p s
    case res of
      Left a -> Left $ fmap f a
      Right (c, t) -> Right (g c, t)

symIf :: (t -> Bool) -> Parser t (Maybe t) t
symIf f = Parser matchSym
  where
    matchSym [] = Left [Nothing]
    matchSym (x : xs)
      | f x = Right (x, xs)
      | otherwise = Left [Just x]

lst :: (Eq t) => [t] -> Parser t (Maybe t) [t]
lst = mapM sym

sym :: (Eq t) => t -> Parser t (Maybe t) t
sym c = symIf (== c)

type StringParser = Parser Char ()
