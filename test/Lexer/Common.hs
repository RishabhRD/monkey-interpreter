module Lexer.Common (module Lexer.Common) where

import Info (Info)
import Lexer (lexer)
import Test.Hspec (shouldBe)
import Token (Token)

testFile :: String -> [Info Token] -> IO ()
testFile fileName expectedTokens = do
  let inputFile = "test/Lexer/" ++ fileName
  input <- readFile inputFile
  lexer input `shouldBe` expectedTokens
