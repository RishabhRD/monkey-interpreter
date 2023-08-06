module Main (main) where

import Lexer (lexer)

main :: IO ()
main = do
  input <- readFile "test/multiline_string.monkey"
  putStr $ show $ lexer input
