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
  , comma
  , number
  , keyword
  , parseFully
  ) where

import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, eof, runParser)
import           Text.Megaparsec.Char       (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error      (ParseErrorBundle (..))

type HiLexer a = (Parsec Void String) a
type HiSkip    = HiLexer ()
type HiSymbol  = HiLexer String
type HiNumber  = HiLexer Rational

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

comma :: HiSymbol
comma = symbol ","

number :: HiNumber
number = toRational <$> lexeme (L.signed skipWhiteSpaces L.scientific)

keyword :: String -> HiSymbol
keyword = lexeme . symbol

symbol :: String -> HiSymbol
symbol = L.symbol skipWhiteSpaces

lexeme :: HiLexer a -> HiLexer a
lexeme = L.lexeme skipWhiteSpaces

skipWhiteSpaces :: HiSkip
skipWhiteSpaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
