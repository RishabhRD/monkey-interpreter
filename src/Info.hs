{-# LANGUAGE DeriveFunctor #-}

module Info (module Info) where

data Info a = Info {val :: a, startRow :: Int, startCol :: Int} deriving (Show, Eq, Functor)
