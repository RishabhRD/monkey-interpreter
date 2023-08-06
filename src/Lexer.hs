module Lexer (lexer) where

import Data.Maybe (fromJust)
import Lexer.Internal (lexParser)
import StringParser (runParser)
import Token (TokenInfo)

lexer :: String -> [TokenInfo]
lexer = fst . fromJust . runParser lexParser
