module Parser.Internal (module Parser.Internal) where

import Control.Applicative (Alternative (many))
import LibParse (Parser)
import Token (TokenInfo)

type TokenParser = Parser TokenInfo

-- statementParser = letParser
--
-- astParser = many statementParser
