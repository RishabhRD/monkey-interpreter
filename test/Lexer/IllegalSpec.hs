module Lexer.IllegalSpec (spec) where

import Info (Info (Info), startCol, startRow, val)
import Lexer (lexer)
import Test.Hspec (Spec, describe, it, shouldBe)
import Token (Token (..))

spec :: Spec
spec = do
  describe "Lexer that runs on a program with illegal characters" $ do
    it "should be able to lex program with illegal characters" $ do
      let expected =
            [ Info {val = Let, startRow = 0, startCol = 0},
              Info {val = Illegal, startRow = 0, startCol = 4},
              Info {val = Illegal, startRow = 0, startCol = 5},
              Info {val = Identifier "f", startRow = 0, startCol = 6},
              Info {val = Assign, startRow = 0, startCol = 8},
              Info {val = Illegal, startRow = 0, startCol = 10},
              Info {val = Integer "2", startRow = 0, startCol = 11},
              Info {val = Semicolon, startRow = 0, startCol = 12}
            ]
      lexer "let \\@f = #2;" `shouldBe` expected
