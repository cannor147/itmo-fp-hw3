module HW3.Parser
  ( parse
  ) where

import           Control.Applicative        (many, optional, (<|>))
import           Control.Monad              (void)
import           Data.Maybe                 (fromMaybe)
import           Data.Void                  (Void)
import           HW3.Base
import           Text.Megaparsec            (Parsec, runParser, eof)
import           Text.Megaparsec.Char       (space1, string)
import           Text.Megaparsec.Char.Lexer (scientific, skipBlockComment,
                                             skipLineComment, space, signed)
import           Text.Megaparsec.Error      (ParseErrorBundle (..))
import Data.Foldable (foldl')

type HiParser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (parseExpression <* eof) mempty

parseExpression :: HiParser HiExpr
parseExpression = do
  value <- skipWhiteSpaces *> parseValue
  applications <- many $ skipWhiteSpaces *> parseApplication
  void skipWhiteSpaces
  return $ foldl' (flip id) value applications

parseValue :: HiParser HiExpr
parseValue = fmap HiExprValue $ parseFunction <|> parseNumeric <|> parseBool

parseApplication :: HiParser (HiExpr -> HiExpr)
parseApplication = do
  void $ skipWhiteSpaces *> string "("
  arguments <- optional $ do
    firstArgument <- parseExpression
    otherArguments <- many $ string "," *> parseExpression
    return $ firstArgument : otherArguments
  void $ skipWhiteSpaces *> string ")"
  return $ \applier -> HiExprApply applier $ fromMaybe [] arguments

parseFunction :: HiParser HiValue
parseFunction = fmap HiValueFunction $
  (string "add"              >> return HiFunAdd)            <|>
  (string "sub"              >> return HiFunSub)            <|>
  (string "mul"              >> return HiFunMul)            <|>
  (string "div"              >> return HiFunDiv)            <|>
  (string "not"              >> return HiFunNot)            <|>
  (string "and"              >> return HiFunAnd)            <|>
  (string "or"               >> return HiFunOr)             <|>
  (string "equals"           >> return HiFunEquals)         <|>
  (string "less-than"        >> return HiFunLessThan)       <|>
  (string "greater-than"     >> return HiFunGreaterThan)    <|>
  (string "not-equals"       >> return HiFunNotEquals)      <|>
  (string "not-less-than"    >> return HiFunNotLessThan)    <|>
  (string "not-greater-than" >> return HiFunNotGreaterThan) <|>
  (string "if"               >> return HiFunIf)

parseNumeric :: HiParser HiValue
parseNumeric = HiValueNumber . toRational <$> signed skipWhiteSpaces scientific

parseBool :: HiParser HiValue
parseBool = fmap HiValueBool $ (string "true" >> return True) <|> (string "false" >> return False)

skipWhiteSpaces :: HiParser ()
skipWhiteSpaces = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")
