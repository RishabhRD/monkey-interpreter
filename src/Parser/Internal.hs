module Parser.Internal (module Parser.Internal) where

-- import Control.Applicative (Alternative (many))
-- import LibParse (Parser (Parser, runParser), symIf)
-- import Token (Token, TokenInfo (token))

-- type TokenParser = Parser TokenInfo ParserError
--
-- parseToken :: Token -> TokenParser Token
-- parseToken expectedToken = Parser $ \tokens -> do
--   let parserWithoutError = token <$> symIf ((== expectedToken) . token)
--   let res = runParser parserWithoutError tokens
--   case res of
--     Right res -> Right res
--     Left _ -> [ParserError {col=_col, row=_row, expectedToken=_expectedToken}]

-- statementParser = letParser
--
-- astParser = many statementParser
