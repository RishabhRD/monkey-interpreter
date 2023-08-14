module Parser.Internal (module Parser.Internal) where

import AST (AST (AST), Expression (..), Ident (Ident), TopStatement (..))
import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (first))
import Info (Info (Info), startCol, startRow, val)
import LibParse (Parser, symIf, symMaybeMap)
import MonadUtils (someWhile)
import Parser.Types (ParseError (UnexpectedEnd, UnexpectedToken))
import Token (Token (..))

type TokenParser = Parser (Info Token) ParseError

toParseError :: Maybe (Info Token) -> ParseError
toParseError Nothing = UnexpectedEnd
toParseError (Just t) = UnexpectedToken t

tokenParser :: Token -> TokenParser (Info Token)
tokenParser expectedToken = first toParseError $ symIf ((== expectedToken) . val)

-- TODO: This seems lot of redundancy, need to explore some elegant syntax
stringTokenParser :: TokenParser (String, Int, Int)
stringTokenParser = first toParseError $ symMaybeMap getVal
  where
    getVal Info {val = StringVal str, startRow = row, startCol = col} = Just (str, row, col)
    getVal _ = Nothing

integerTokenParser :: TokenParser (String, Int, Int)
integerTokenParser = first toParseError $ symMaybeMap getVal
  where
    getVal Info {val = Integer str, startRow = row, startCol = col} = Just (str, row, col)
    getVal _ = Nothing

identifierTokenParser :: TokenParser (String, Int, Int)
identifierTokenParser = first toParseError $ symMaybeMap getVal
  where
    getVal Info {val = Identifier str, startRow = row, startCol = col} = Just (str, row, col)
    getVal _ = Nothing

stringParser :: TokenParser (Info Expression)
stringParser = do
  (str, row, col) <- stringTokenParser
  let node = Info {val = str, startRow = row, startCol = col}
  return Info {val = StringNode node, startRow = row, startCol = col}

integerParser :: TokenParser (Info Expression)
integerParser = do
  (str, row, col) <- integerTokenParser
  let node = Info {val = str, startRow = row, startCol = col}
  return Info {val = IntegerNode node, startRow = row, startCol = col}

variableParser :: TokenParser (Info Expression)
variableParser = do
  (str, row, col) <- identifierTokenParser
  let node = Info {val = Ident str, startRow = row, startCol = col}
  return Info {val = VariableNode node, startRow = row, startCol = col}

expressionParser :: Parser (Info Token) ParseError (Info Expression)
expressionParser =
  stringParser
    <|> integerParser
    <|> variableParser

letParser :: TokenParser (Info TopStatement)
letParser = do
  letT <- tokenParser Let
  (identToken, ir, ic) <- identifierTokenParser
  let ident = Info {val = Ident identToken, startRow = ir, startCol = ic}
  _ <- tokenParser Assign
  expr <- expressionParser
  _ <- tokenParser Semicolon
  return Info {val = LetStatement ident expr, startRow = startRow letT, startCol = startCol letT}

topStatementParser :: TokenParser (Info TopStatement)
topStatementParser = letParser <|> token2Node EOFNode EOF
  where
    token2Node node token = fmap (const node) <$> tokenParser token

astParser :: TokenParser AST
astParser = AST <$> someWhile (isNotEOF . val) topStatementParser
  where
    isNotEOF EOFNode = False
    isNotEOF _ = True
