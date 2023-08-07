module Lexer.Common (module Lexer.Common) where

import Lexer
import Test.Hspec
import Token

testFile :: String -> [TokenInfo] -> IO ()
testFile fileName expectedTokens = do
  let inputFile = "test/Lexer/" ++ fileName
  input <- readFile inputFile
  lexer input `shouldBe` expectedTokens
