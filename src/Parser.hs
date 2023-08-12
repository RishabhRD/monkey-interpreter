module Parser (module Parser) where

import AST (AST)
import Info (Info)
import LibParse (Parser (runParser))
import Parser.Internal (astParser)
import Parser.Types (ParseError (..))
import Token (Token)

parse :: [Info Token] -> Either [ParseError] AST
parse = fmap fst . runParser astParser
