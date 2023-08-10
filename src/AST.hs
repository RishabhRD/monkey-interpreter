module AST (module AST) where

newtype Ident = Ident String deriving (Show, Eq)

data Expression = Var Ident

data Statement
  = Let Ident Expression
  | Return Expression

newtype Program = Program [Statement]
