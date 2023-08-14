module Lexer (lexer) where

import Data.Either (fromRight)
import Info (Info)
import Lexer.Internal (lexParser)
import LibParse (runParser)
import Token (Token)

lexer :: String -> [Info Token]
lexer str = fst . fromRight ([], "") . runParser lexParser $ (str ++ "\0")
