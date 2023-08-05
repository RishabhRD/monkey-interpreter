module Lexer.Internal (module Lexer.Internal) where

import Control.Applicative (Alternative (many, some), (<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Maybe (fromJust)
import StringParser (Parser (Parser, runParser), char, charIf, string)
import Token (Token (..))

singleWhitespaceParser :: Parser Int
singleWhitespaceParser = fmap (const 1) $ char ' ' <|> char '\t'

whitespaceLengthParser :: Parser Int
whitespaceLengthParser = length <$> many singleWhitespaceParser

integerParser :: Parser (Token, Int)
integerParser = toTokenWithLength <$> some (charIf isDigit)
  where
    toTokenWithLength lst = (Integer lst, length lst)

identifierParser :: Parser (Token, Int)
identifierParser = toTokenWithLength <$> Parser matchString
  where
    matchString s = do
      (headEle, s1) <- runParser (charIf isAlpha) s
      (tailList, remString) <- runParser (many alphaNumeric) s1
      return (headEle : tailList, remString)
      where
        alphaNumeric = charIf isAlphaNum <|> char '_'
    toTokenWithLength lst = (Integer lst, length lst)

hardcodeToken :: [Char] -> Token -> Parser (Token, Int)
hardcodeToken str token = (token, length str) <$ string str

illegalParser :: Parser (Token, Int)
illegalParser = (Illegal, 1) <$ charIf (const True)

singleTokenParser :: Parser (Token, Int)
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

singleTokenWithWhitespace :: Parser (Token, (Int, Int))
singleTokenWithWhitespace = fmap isomorph $ (,) <$> whitespaceLengthParser <*> singleTokenParser
  where
    isomorph (a, (b, c)) = (b, (a, c))

toTokenLineInfo :: [(Token, (Int, Int))] -> [(Token, Int)]
toTokenLineInfo tokenInfos = zip tokens startPoints
  where
    tokens = fst <$> tokenInfos
    positions = snd <$> tokenInfos
    whiteSpaceToStartPoint (prevStart, prevSpace) (curWhite, curSpace) =
      (prevStart + prevSpace + curWhite, curSpace)
    startPoints = fst <$> scanl whiteSpaceToStartPoint (0, 0) positions

lineParser :: Parser [(Token, Int)]
lineParser = toTokenLineInfo <$> many singleTokenWithWhitespace

lexLine :: String -> [(Token, Int)]
lexLine = fst . fromJust . runParser lineParser
