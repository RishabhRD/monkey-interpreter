module AST (module AST) where

data ParseError = ParseError

newtype Ident = Ident String deriving (Show, Eq)

type NodeInfo a = (a, Int, Int)

data Expression = Var (NodeInfo Ident)

data Statement
  = Let Ident Expression
  | Return Expression

newtype AST = AST [NodeInfo Statement]
