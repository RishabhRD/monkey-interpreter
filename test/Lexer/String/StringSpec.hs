module Lexer.String.StringSpec (spec) where

import Info (Info (..), startCol, startRow, val)
import Lexer.Common (testFile)
import Test.Hspec (Spec, describe, it)
import Token (Token (..))

spec :: Spec
spec = do
  describe "Lexing string literal" $ do
    it "should be able to lex simple single line string" $ do
      let expected =
            [ Info
                { val = Let,
                  startRow = 0,
                  startCol = 0
                },
              Info
                { val = Identifier "x",
                  startRow = 0,
                  startCol = 4
                },
              Info
                { val = Assign,
                  startRow = 0,
                  startCol = 6
                },
              Info
                { val = StringVal "\"hello\"",
                  startRow = 0,
                  startCol = 8
                },
              Info
                { val = Semicolon,
                  startRow = 0,
                  startCol = 15
                }
            ]
      testFile "String/simple_string.monkey" expected
    it "should be able to lex string with \\\" inside it" $ do
      let expected =
            [ Info
                { val = Let,
                  startRow = 0,
                  startCol = 0
                },
              Info
                { val = Identifier "x",
                  startRow = 0,
                  startCol = 4
                },
              Info
                { val = Assign,
                  startRow = 0,
                  startCol = 6
                },
              Info
                { val = StringVal "\"hello \\\"world\\\"\"",
                  startRow = 0,
                  startCol = 8
                },
              Info
                { val = Semicolon,
                  startRow = 0,
                  startCol = 25
                }
            ]
      testFile "String/string_with_backslash_quotes.monkey" expected
    it "should be able to detect newline in string" $ do
      let expected =
            [ Info
                { val = Let,
                  startRow = 0,
                  startCol = 0
                },
              Info
                { val = Identifier "x",
                  startRow = 0,
                  startCol = 4
                },
              Info
                { val = Assign,
                  startRow = 0,
                  startCol = 6
                },
              Info
                { val = StringVal "\"hello\n  world\"",
                  startRow = 0,
                  startCol = 8
                },
              Info
                { val = Semicolon,
                  startRow = 1,
                  startCol = 8
                }
            ]
      testFile "String/newline.monkey" expected
    it "should be able to mixable with other vals" $ do
      let expected =
            [ Info
                { val = Let,
                  startRow = 0,
                  startCol = 0
                },
              Info
                { val = Identifier "x",
                  startRow = 0,
                  startCol = 4
                },
              Info
                { val = Assign,
                  startRow = 0,
                  startCol = 6
                },
              Info
                { val = StringVal "\"hello\n  \\\"world\\\"\"",
                  startRow = 0,
                  startCol = 8
                },
              Info
                { val = Semicolon,
                  startRow = 1,
                  startCol = 12
                },
              Info
                { val = Let,
                  startRow = 1,
                  startCol = 14
                },
              Info
                { val = Identifier "y",
                  startRow = 1,
                  startCol = 18
                },
              Info
                { val = Assign,
                  startRow = 1,
                  startCol = 19
                },
              Info
                { val = Integer "5",
                  startRow = 1,
                  startCol = 21
                },
              Info
                { val = Semicolon,
                  startRow = 1,
                  startCol = 22
                },
              Info
                { val = Let,
                  startRow = 2,
                  startCol = 0
                },
              Info
                { val = Identifier "z",
                  startRow = 2,
                  startCol = 4
                },
              Info
                { val = Assign,
                  startRow = 2,
                  startCol = 6
                },
              Info
                { val = Integer "2",
                  startRow = 2,
                  startCol = 8
                },
              Info
                { val = Semicolon,
                  startRow = 2,
                  startCol = 9
                }
            ]
      testFile "String/string_with_multiple_statements.monkey" expected
