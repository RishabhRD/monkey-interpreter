module Lexer.String.StringSpec (spec) where

import Lexer.Common
import Test.Hspec (Spec, describe, it)
import Token (Token (..), TokenInfo (..))

spec :: Spec
spec = do
  describe "Lexing string literal" $ do
    it "should be able to lex simple single line string" $ do
      let expected =
            [ TokenInfo
                { token = Let,
                  startRow = 0,
                  startCol = 0
                },
              TokenInfo
                { token = Identifier "x",
                  startRow = 0,
                  startCol = 4
                },
              TokenInfo
                { token = Assign,
                  startRow = 0,
                  startCol = 6
                },
              TokenInfo
                { token = StringVal "\"hello\"",
                  startRow = 0,
                  startCol = 8
                },
              TokenInfo
                { token = Semicolon,
                  startRow = 0,
                  startCol = 15
                }
            ]
      testFile "String/simple_string.monkey" expected
    it "should be able to lex string with \\\" inside it" $ do
      let expected =
            [ TokenInfo
                { token = Let,
                  startRow = 0,
                  startCol = 0
                },
              TokenInfo
                { token = Identifier "x",
                  startRow = 0,
                  startCol = 4
                },
              TokenInfo
                { token = Assign,
                  startRow = 0,
                  startCol = 6
                },
              TokenInfo
                { token = StringVal "\"hello \\\"world\\\"\"",
                  startRow = 0,
                  startCol = 8
                },
              TokenInfo
                { token = Semicolon,
                  startRow = 0,
                  startCol = 25
                }
            ]
      testFile "String/string_with_backslash_quotes.monkey" expected
    it "should be able to detect newline in string" $ do
      let expected =
            [ TokenInfo
                { token = Let,
                  startRow = 0,
                  startCol = 0
                },
              TokenInfo
                { token = Identifier "x",
                  startRow = 0,
                  startCol = 4
                },
              TokenInfo
                { token = Assign,
                  startRow = 0,
                  startCol = 6
                },
              TokenInfo
                { token = StringVal "\"hello\n  world\"",
                  startRow = 0,
                  startCol = 8
                },
              TokenInfo
                { token = Semicolon,
                  startRow = 1,
                  startCol = 8
                }
            ]
      testFile "String/newline.monkey" expected
    it "should be able to mixable with other tokens" $ do
      let expected =
            [ TokenInfo
                { token = Let,
                  startRow = 0,
                  startCol = 0
                },
              TokenInfo
                { token = Identifier "x",
                  startRow = 0,
                  startCol = 4
                },
              TokenInfo
                { token = Assign,
                  startRow = 0,
                  startCol = 6
                },
              TokenInfo
                { token = StringVal "\"hello\n  \\\"world\\\"\"",
                  startRow = 0,
                  startCol = 8
                },
              TokenInfo
                { token = Semicolon,
                  startRow = 1,
                  startCol = 12
                },
              TokenInfo
                { token = Let,
                  startRow = 1,
                  startCol = 14
                },
              TokenInfo
                { token = Identifier "y",
                  startRow = 1,
                  startCol = 18
                },
              TokenInfo
                { token = Assign,
                  startRow = 1,
                  startCol = 19
                },
              TokenInfo
                { token = Integer "5",
                  startRow = 1,
                  startCol = 21
                },
              TokenInfo
                { token = Semicolon,
                  startRow = 1,
                  startCol = 22
                },
              TokenInfo
                { token = Let,
                  startRow = 2,
                  startCol = 0
                },
              TokenInfo
                { token = Identifier "z",
                  startRow = 2,
                  startCol = 4
                },
              TokenInfo
                { token = Assign,
                  startRow = 2,
                  startCol = 6
                },
              TokenInfo
                { token = Integer "2",
                  startRow = 2,
                  startCol = 8
                },
              TokenInfo
                { token = Semicolon,
                  startRow = 2,
                  startCol = 9
                }
            ]
      testFile "String/string_with_multiple_statements.monkey" expected
