{-# LANGUAGE TupleSections #-}

module Lexer.Internal (module Lexer.Internal) where

import Control.Applicative (Alternative (many, some), (<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import StringParser (Parser (Parser, runParser), char, charIf, string)
import Token (Token (..), TokenInfo (TokenInfo, startCol, startRow, token))

type Rows = Int

type Cols = Int

singleWhitespaceParser :: Parser Cols
singleWhitespaceParser = fmap (const 1) $ char ' ' <|> char '\t'

singleNewLineParser :: Parser Rows
singleNewLineParser = fmap (const 1) $ string "\n" <|> string "\r\n"

whiteSpaceEater :: Parser (Rows, Cols)
whiteSpaceEater = toRowCol <$> blankPositions
  where
    blankPositions = many $ ((,0) <$> singleNewLineParser) <|> ((0,) <$> singleWhitespaceParser)
    toRowCol = foldl combineRowCol (0, 0)
    combineRowCol (a, b) (0, d) = (a, b + d)
    combineRowCol (a, b) (c, _) = (a + c, b)

integerParser :: Parser (Token, Rows, Cols)
integerParser = toTokenWithLength <$> some (charIf isDigit)
  where
    toTokenWithLength lst = (Integer lst, 0, length lst)

identifierParser :: Parser (Token, Rows, Cols)
identifierParser = toTokenWithLength <$> Parser matchString
  where
    matchString s = do
      (headEle, s1) <- runParser (charIf isAlpha) s
      (tailList, remString) <- runParser (many alphaNumeric) s1
      return (headEle : tailList, remString)
      where
        alphaNumeric = charIf isAlphaNum <|> char '_'
    toTokenWithLength lst = (Integer lst, 0, length lst)

hardcodeToken :: [Char] -> Token -> Parser (Token, Rows, Cols)
hardcodeToken str t = (t, 0, length str) <$ string str

illegalParser :: Parser (Token, Rows, Cols)
illegalParser = (Illegal, 0, 1) <$ charIf (const True)

singleTokenParser :: Parser (Token, Rows, Cols)
singleTokenParser =
  hardcodeToken "==" Equals
    <|> hardcodeToken "=" Assign
    <|> hardcodeToken "&&" And
    <|> hardcodeToken "&" BitAnd
    <|> hardcodeToken "||" Or
    <|> hardcodeToken "|" BitOr
    <|> hardcodeToken "!" Not
    <|> hardcodeToken "~" BitNegate
    <|> hardcodeToken "~" BitXor
    <|> hardcodeToken "<=" LessEq
    <|> hardcodeToken "<" Less
    <|> hardcodeToken ">=" GreaterEq
    <|> hardcodeToken ">" Greater
    <|> hardcodeToken "+" Plus
    <|> hardcodeToken "-" Minus
    <|> hardcodeToken "*" Asterisk
    <|> hardcodeToken "/" Slash
    <|> hardcodeToken "%" Mod
    <|> hardcodeToken "," Comma
    <|> hardcodeToken ";" Semicolon
    <|> hardcodeToken "(" LParen
    <|> hardcodeToken ")" RParen
    <|> hardcodeToken "{" LBrace
    <|> hardcodeToken "}" RBrace
    <|> hardcodeToken "[" LBracket
    <|> hardcodeToken "]" RBracket
    <|> hardcodeToken "fn" Function
    <|> hardcodeToken "let" Let
    <|> hardcodeToken "if" If
    <|> hardcodeToken "else" Else
    <|> hardcodeToken "true" TrueVal
    <|> hardcodeToken "false" FalseVal
    <|> hardcodeToken "return" Return
    <|> integerParser
    <|> identifierParser
    <|> illegalParser

singleTokenWithWhitespace :: Parser (Token, ((Rows, Rows), (Cols, Cols)))
singleTokenWithWhitespace = fmap isomorph $ (,) <$> whiteSpaceEater <*> singleTokenParser
  where
    isomorph ((a, b), (c, d, e)) = (c, ((a, d), (b, e)))

toTokenLineInfo :: [(Token, ((Rows, Rows), (Cols, Cols)))] -> [TokenInfo]
toTokenLineInfo tokenInfos = isomorph <$> zip tokens startPoints
  where
    tokens = fst <$> tokenInfos
    positions = snd <$> tokenInfos
    toStartPoint
      ((prevRowCovered, prevRowOccupied), (prevColCovered, prevColOccupied))
      ((0, curRowOccupied), (curColBefore, curColOccupied)) =
        ( (prevRowCovered + prevRowOccupied, curRowOccupied),
          (prevColCovered + prevColOccupied + curColBefore, curColOccupied)
        )
    toStartPoint
      ((prevRowCovered, prevRowOccupied), (_, _))
      ((curRowBefore, curRowOccupied), (curColBefore, curColOccupied)) =
        ( (prevRowCovered + prevRowOccupied + curRowBefore, curRowOccupied),
          (curColBefore, curColOccupied)
        )
    startPoints = tail (scanl toStartPoint ((0, 0), (0, 0)) positions)
    isomorph (a, ((b, _), (c, _))) = TokenInfo {token = a, startRow = b, startCol = c}

lexParser :: Parser [TokenInfo]
lexParser = toTokenLineInfo <$> many singleTokenWithWhitespace
