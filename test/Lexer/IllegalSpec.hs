module Lexer.IllegalSpec (spec) where

import Lexer
import Test.Hspec (Spec, describe, it, shouldBe)
import Token (Token (..), TokenInfo (..))

spec :: Spec
spec = do
  describe "Lexer that runs on a program with illegal characters" $ do
    it "should be able to lex program with illegal characters" $ do
      let expected =
            [ TokenInfo {token = Let, startRow = 0, startCol = 0},
              TokenInfo {token = Illegal, startRow = 0, startCol = 4},
              TokenInfo {token = Illegal, startRow = 0, startCol = 5},
              TokenInfo {token = Identifier "f", startRow = 0, startCol = 6},
              TokenInfo {token = Assign, startRow = 0, startCol = 8},
              TokenInfo {token = Illegal, startRow = 0, startCol = 10},
              TokenInfo {token = Integer "2", startRow = 0, startCol = 11},
              TokenInfo {token = Semicolon, startRow = 0, startCol = 12}
            ]
      lexer "let \\@f = #2;" `shouldBe` expected
