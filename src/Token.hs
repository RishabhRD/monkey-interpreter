module Token (module Token) where

data Token
  = Illegal
  | Identifier String
  | Integer String
  | StringVal String
  | -- Symbols
    Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | -- Operator
    Plus
  | Minus
  | Asterisk
  | Slash
  | Mod
  | Assign
  | -- Bit Operations
    BitAnd
  | BitOr
  | BitXor
  | BitNegate
  | -- Logical Operations
    And
  | Or
  | Not
  | -- Comparision
    Less
  | LessEq
  | Greater
  | GreaterEq
  | Equals
  | -- Keywords
    Function
  | Let
  | If
  | Else
  | TrueVal
  | FalseVal
  | Return
  deriving (Show, Eq)

data TokenInfo = TokenInfo {token :: Token, startRow :: Int, startCol :: Int}
  deriving (Show, Eq)
