module Parser (module Parser) where

import AST (AST, ParseError (..))
import Token (TokenInfo)

parse :: [TokenInfo] -> Either ParseError AST
parse = const $ Left ParseError
