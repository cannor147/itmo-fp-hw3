module HW3.Lexer
  ( HiLexer
  , HiSkip
  , asterisk
  , boolAnd
  , boolOr
  , byte
  , close
  , closeBrace
  , closeBytes
  , closeSquare
  , colon
  , comma
  , dot
  , eq
  , excl
  , ge
  , gt
  , keyword
  , le
  , lt
  , minus
  , neq
  , number
  , open
  , openBrace
  , openBytes
  , openSquare
  , parseFully
  , plus
  , slash
  , string
  , word
  ) where

import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Data.Word                  (Word8)
import           Text.Megaparsec            (Parsec, eof, manyTill, runParser, some, notFollowedBy, try)
import           Text.Megaparsec.Char       (char, space1, alphaNumChar, hexDigitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error      (ParseErrorBundle (..))

type HiLexer a = (Parsec Void String) a
type HiSkip    = HiLexer ()
type HiSymbol  = HiLexer String
type HiNumber  = HiLexer Rational
type HiString  = HiLexer Text
type HiByte    = HiLexer Word8

-- | Wraps parser with skipping whitespaces and eof.
parseFully :: HiLexer a -> String -> Either (ParseErrorBundle String Void) a
parseFully parser = runParser (skipWhiteSpaces *> parser <* eof) mempty

-- | Lexer for asterisk.
asterisk :: HiSymbol
asterisk = symbol "*"

-- | Lexer for slash.
slash :: HiSymbol
slash = try $ symbol "/" <* notFollowedBy (symbol "=")

-- | Lexer for plus.
plus :: HiSymbol
plus = symbol "+"

-- | Lexer for minus.
minus :: HiSymbol
minus = symbol "-"

-- | Lexer for eq.
eq :: HiSymbol
eq = symbol "=="

-- | Lexer for neq.
neq :: HiSymbol
neq = symbol "/="

-- | Lexer for ge.
ge :: HiSymbol
ge = symbol ">="

-- | Lexer for le.
le :: HiSymbol
le = symbol "<="

-- | Lexer for gt.
gt :: HiSymbol
gt = symbol ">"

-- | Lexer for lt.
lt :: HiSymbol
lt = symbol "<"

-- | Lexer for and.
boolAnd :: HiSymbol
boolAnd = symbol "&&"

-- | Lexer for or.
boolOr :: HiSymbol
boolOr = symbol "||"

-- | Lexer for opening bracket.
open :: HiSymbol
open = symbol "("

-- | Lexer for closing bracket.
close :: HiSymbol
close = symbol ")"

-- | Lexer for opening square bracket.
openSquare :: HiSymbol
openSquare = symbol "["

-- | Lexer for closing square bracket.
closeSquare :: HiSymbol
closeSquare = symbol "]"

-- | Lexer for opening brace.
openBrace :: HiSymbol
openBrace = symbol "{"

-- | Lexer for closing brace.
closeBrace :: HiSymbol
closeBrace = symbol "}"

-- | Lexer for opening bracket and href.
openBytes :: HiSymbol
openBytes = symbol "[#"

-- | Lexer for closing bracket and href.
closeBytes :: HiSymbol
closeBytes = symbol "#]"

-- | Lexer for comma.
comma :: HiSymbol
comma = symbol ","

-- | Lexer for colon.
colon :: HiSymbol
colon = symbol ":"

-- | Lexer for dot.
dot :: HiSymbol
dot = symbol "."

-- | Lexer for exclamation mark.
excl :: HiSymbol
excl = symbol "!"

-- | Lexer for number.
number :: HiNumber
number = toRational <$> lexeme (L.signed skipWhiteSpaces L.scientific)

-- | Lexer for string.
string :: HiString
string = pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

-- | Lexer for keyword.
keyword :: String -> HiSymbol
keyword = lexeme . symbol

-- | Lexer for word.
word :: HiSymbol
word = lexeme $ some alphaNumChar

-- | Lexer for byte.
byte :: HiByte
byte = lexeme $ do
  x <- hexDigitChar
  y <- try hexDigitChar <* notFollowedBy hexDigitChar
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

-- | Lexer for symbol.
symbol :: String -> HiSymbol
symbol = L.symbol skipWhiteSpaces

lexeme :: HiLexer a -> HiLexer a
lexeme = L.lexeme skipWhiteSpaces

skipWhiteSpaces :: HiSkip
skipWhiteSpaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
