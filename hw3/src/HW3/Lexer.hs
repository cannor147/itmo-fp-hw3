module HW3.Lexer
  ( HiSkip
  , HiLexer
  , asterisk
  , slash
  , plus
  , minus
  , eq
  , neq
  , ge
  , le
  , gt
  , lt
  , boolAnd
  , boolOr
  , open
  , close
  , openSquare
  , closeSquare
  , openBrace
  , closeBrace
  , openBytes
  , closeBytes
  , comma
  , colon
  , dot
  , number
  , word
  , byte
  , string
  , keyword
  , parseFully
  ) where

import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Data.Word                  (Word8)
import           Text.Megaparsec            (Parsec, eof, manyTill, runParser, some)
import           Text.Megaparsec.Char       (char, space1, alphaNumChar, hexDigitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error      (ParseErrorBundle (..))

type HiLexer a = (Parsec Void String) a
type HiSkip    = HiLexer ()
type HiSymbol  = HiLexer String
type HiNumber  = HiLexer Rational
type HiString  = HiLexer Text
type HiByte    = HiLexer Word8

parseFully :: HiLexer a -> String -> Either (ParseErrorBundle String Void) a
parseFully parser = runParser (skipWhiteSpaces *> parser <* eof) mempty

asterisk :: HiSymbol
asterisk = symbol "*"

slash :: HiSymbol
slash = symbol "/"

plus :: HiSymbol
plus = symbol "+"

minus :: HiSymbol
minus = symbol "-"

eq :: HiSymbol
eq = symbol "=="

neq :: HiSymbol
neq = symbol "/="

ge :: HiSymbol
ge = symbol ">="

le :: HiSymbol
le = symbol "<="

gt :: HiSymbol
gt = symbol ">"

lt :: HiSymbol
lt = symbol "<"

boolAnd :: HiSymbol
boolAnd = symbol "&&"

boolOr :: HiSymbol
boolOr = symbol "||"

open :: HiSymbol
open = symbol "("

close :: HiSymbol
close = symbol ")"

openSquare :: HiSymbol
openSquare = symbol "["

closeSquare :: HiSymbol
closeSquare = symbol "]"

openBrace :: HiSymbol
openBrace = symbol "{"

closeBrace :: HiSymbol
closeBrace = symbol "}"

openBytes :: HiSymbol
openBytes = symbol "[#"

closeBytes :: HiSymbol
closeBytes = symbol "#]"

comma :: HiSymbol
comma = symbol ","

colon :: HiSymbol
colon = symbol ":"

dot :: HiSymbol
dot = symbol "."

number :: HiNumber
number = toRational <$> lexeme (L.signed skipWhiteSpaces L.scientific)

string :: HiString
string = pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

keyword :: String -> HiSymbol
keyword = lexeme . symbol

word :: HiSymbol
word = lexeme $ some alphaNumChar

byte :: HiByte
byte = lexeme $ do
  x <- hexDigitChar
  y <- hexDigitChar
  return $ charToInt x * 16 + charToInt y
    where
      charToInt ch
        | ch == '1'              = 1
        | ch == '2'              = 2
        | ch == '3'              = 3
        | ch == '4'              = 4
        | ch == '5'              = 5
        | ch == '6'              = 6
        | ch == '7'              = 7
        | ch == '8'              = 8
        | ch == '9'              = 9
        | ch == 'A' || ch == 'a' = 10
        | ch == 'B' || ch == 'b' = 11
        | ch == 'C' || ch == 'c' = 12
        | ch == 'D' || ch == 'd' = 13
        | ch == 'E' || ch == 'e' = 14
        | ch == 'F' || ch == 'f' = 15
        | otherwise              = 0

symbol :: String -> HiSymbol
symbol = L.symbol skipWhiteSpaces

lexeme :: HiLexer a -> HiLexer a
lexeme = L.lexeme skipWhiteSpaces

skipWhiteSpaces :: HiSkip
skipWhiteSpaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
