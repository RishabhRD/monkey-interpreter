module Parser.Types (module Parser.Types) where

import Token (TokenInfo)

data ParserError
  = UnexpectedToken TokenInfo
  | UnexpectedEnd
