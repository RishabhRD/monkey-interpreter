module AST (module AST) where

import Info (Info (..))

newtype Ident = Ident String deriving (Show, Eq)

data LetStatement = LetStatement (Info Ident) (Info Expression) deriving (Show, Eq)

data Expression
  = VariableNode (Info Ident)
  | IntegerNode (Info String)
  | StringNode (Info String)
  deriving (Show, Eq)

newtype AST = AST [Info LetStatement] deriving (Show, Eq)
