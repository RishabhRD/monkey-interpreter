module Lexer (lexer) where

import Lexer.Internal (lexLine)
import Token (TokenInfo (..))

lexer :: [String] -> [TokenInfo]
lexer fileLines = zip [1 ..] (lexLine <$> fileLines) >>= addRowInfo
  where
    addRowInfo (row, tokens) = isomorph <$> fmap (row,) tokens
    isomorph (row, (token, col)) = TokenInfo {token = token, startRow = row, startCol = col}
