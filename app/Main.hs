module Main (main) where

import Lexer (lexer)

main :: IO ()
main = do
  putStr $ show $ lexer "let x = 5;\nlet y = 3;"
