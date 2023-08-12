module Parser.Types (module Parser.Types) where

import Info (Info)
import Token (Token)

data ParseError
  = UnexpectedToken (Info Token)
  | UnexpectedEnd
