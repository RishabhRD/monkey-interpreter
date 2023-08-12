{-# LANGUAGE TupleSections #-}

module Lexer.Internal (module Lexer.Internal) where

import Control.Applicative (Alternative (many, some), (<|>))
import Data.Bifunctor (Bifunctor, first)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Info (Info (Info, startCol, startRow, val))
import LibParse (Parser (Parser), lst, runParser, sym, symIf)
import Token (Token (..))

type StringParser = Parser Char ()

type Rows = Int

type Cols = Int

firstToUnit :: (Data.Bifunctor.Bifunctor p) => (t -> p a c) -> t -> p () c
firstToUnit p c = first (const ()) (p c)

char :: Char -> Parser Char () Char
char = firstToUnit sym

charIf :: (c -> Bool) -> Parser c () c
charIf = firstToUnit symIf

string :: String -> Parser Char () String
string = firstToUnit lst

singleWhitespaceParser :: StringParser Cols
singleWhitespaceParser = fmap (const 1) $ char ' ' <|> char '\t'

singleNewLineParser :: StringParser Rows
singleNewLineParser = fmap (const 1) $ string "\n" <|> string "\r\n"

whiteSpaceEater :: StringParser (Rows, Cols)
whiteSpaceEater = toRowCol <$> blankPositions
  where
    blankPositions = many $ ((,0) <$> singleNewLineParser) <|> ((0,) <$> singleWhitespaceParser)
    toRowCol = foldl combineRowCol (0, 0)
    combineRowCol (a, b) (0, d) = (a, b + d)
    combineRowCol (a, b) (c, _) = (a + c, b)

integerParser :: StringParser (Token, Rows, Cols)
integerParser = toTokenWithLength <$> some (charIf isDigit)
  where
    toTokenWithLength input = (Integer input, 0, length input)

identifierParser :: StringParser (Token, Rows, Cols)
identifierParser = toTokenWithLength <$> Parser matchString
  where
    matchString s = do
      (headEle, s1) <- runParser (charIf isAlpha) s
      (tailList, remString) <- runParser (many alphaNumeric) s1
      return (headEle : tailList, remString)
      where
        alphaNumeric = charIf isAlphaNum <|> char '_'
    toTokenWithLength input = (Identifier input, 0, length input)

hardcodeToken :: [Char] -> Token -> StringParser (Token, Rows, Cols)
hardcodeToken str t = (t, 0, length str) <$ string str

stringParser :: StringParser (Token, Int, Int)
stringParser = fmap isomorph (combineAll <$> quotesParser <*> stringParser' <*> quotesParser)
  where
    quotesParser = char '"'
    stringParser' = many $ backSlashQuotes <|> newLineParser <|> anyExceptQuotes
    backSlashQuotes = singleRowTransform <$> string "\\\""
    newLineParser = (,(1, 0)) <$> (string "\n" <|> string "\r\n")
    singleRowTransform str = (str, (0, length str))
    anyExceptQuotes = (,(0, 1)) . (: []) <$> charIf (/= '"')
    combineAll _ strings' _ = (resultantString, foldl calcSpace (0, 0) positions)
      where
        strings = ("\"", (0, 1)) : strings' ++ [("\"", (0, 1))]
        resultantString = strings >>= fst
        positions = snd <$> strings
        calcSpace (rowCon, colCon) (0, curCol) = (rowCon, colCon + curCol)
        calcSpace (rowCon, _) (curRow, curCol) = (rowCon + curRow, curCol)
    isomorph (a, (b, c)) = (StringVal a, b, c)

illegalParser :: StringParser (Token, Rows, Cols)
illegalParser = (Illegal, 0, 1) <$ charIf (const True)

singleTokenParser :: StringParser (Token, Rows, Cols)
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
    <|> stringParser
    <|> integerParser
    <|> identifierParser
    <|> illegalParser

singleTokenWithWhitespace :: StringParser (Token, ((Rows, Rows), (Cols, Cols)))
singleTokenWithWhitespace = fmap isomorph $ (,) <$> whiteSpaceEater <*> singleTokenParser
  where
    isomorph ((a, b), (c, d, e)) = (c, ((a, d), (b, e)))

toTokenLineInfo :: [(Token, ((Rows, Rows), (Cols, Cols)))] -> [Info Token]
toTokenLineInfo tokenInfos = isomorph <$> zip tokens startPoints
  where
    tokens = fst <$> tokenInfos
    positions = snd <$> tokenInfos
    toStartPoint
      ((prevRowCovered, prevRowOccupied), (prevColCovered, prevColOccupied))
      ((0, curRowOccupied), (curColBefore, curColOccupied)) =
        ( (prevRowCovered + prevRowOccupied, curRowOccupied),
          (extraCols + curColBefore, curColOccupied)
        )
        where
          extraCols =
            if prevRowOccupied > 0
              then prevColOccupied
              else prevColCovered + prevColOccupied
    toStartPoint
      ((prevRowCovered, prevRowOccupied), (_, _))
      ((curRowBefore, curRowOccupied), (curColBefore, curColOccupied)) =
        ( (prevRowCovered + prevRowOccupied + curRowBefore, curRowOccupied),
          (curColBefore, curColOccupied)
        )
    startPoints = tail (scanl toStartPoint ((0, 0), (0, 0)) positions)
    isomorph (a, ((b, _), (c, _))) = Info {val = a, startRow = b, startCol = c}

lexParser :: StringParser [Info Token]
lexParser = toTokenLineInfo <$> many singleTokenWithWhitespace
