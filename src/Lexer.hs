module Lexer (lexer) where

import Data.Either (fromRight)
import Lexer.Internal (lexParser)
import LibParse (runParser)
import Token (TokenInfo)

lexer :: String -> [TokenInfo]
lexer = fst . fromRight ([], "") . runParser lexParser
