module Main (main) where

import Control.Monad (forever)
import Lexer (lexer)
import Parser (parse)
import System.IO (hFlush, stdout)

shell :: String -> IO String
shell prev = do
  putStrLn ""
  putStr "File name (Empty for previous) >>> "
  hFlush stdout
  inputFile <- getLine
  let fileToLex = case inputFile of
        "" -> prev
        _ -> inputFile
  input <- readFile fileToLex
  putStrLn ""
  putStrLn "----------------------------"
  let lexed = lexer input
  print lexed
  print ""
  print $ parse lexed
  putStrLn "----------------------------"
  return fileToLex

main :: IO ()
main = forever $ do
  _ <- foldForFiles (return "") shell
  putStrLn "Shell program finished."

foldForFiles :: IO String -> (String -> IO String) -> IO String
foldForFiles a f = do
  curFile <- a
  newFile <- f curFile
  foldForFiles (return newFile) f
