module HW3.Parser
  ( parse
  ) where

import           Control.Applicative        (many, optional, (<|>))
import           Control.Monad              (void)
import           Data.Maybe                 (fromMaybe)
import           Data.Void                  (Void)
import           HW3.Base
import           Text.Megaparsec            (Parsec, runParser)
import           Text.Megaparsec.Char       (space1, string)
import           Text.Megaparsec.Char.Lexer (scientific, skipBlockComment,
                                             skipLineComment, space)
import           Text.Megaparsec.Error      (ParseErrorBundle (..))

type HiParser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (skipWhiteSpaces *> parseExpression <* skipWhiteSpaces) mempty

parseFunction :: HiParser HiValue
parseFunction = fmap HiValueFunction $
  (string "add" >> return HiFunAdd) <|>
  (string "sub" >> return HiFunSub) <|>
  (string "mul" >> return HiFunMul) <|>
  (string "div" >> return HiFunDiv)

parseNumeric :: HiParser HiValue
parseNumeric = HiValueNumber . toRational <$> scientific

parseApplication :: HiParser HiExpr
parseApplication = do
  function <- skipWhiteSpaces *> parseFunction
  void $ skipWhiteSpaces *> string "("
  arguments <- optional $ do
    firstArgument <- skipWhiteSpaces *> parseExpression
    otherArguments <- many $ skipWhiteSpaces *> string "," *> skipWhiteSpaces *> parseExpression
    return $ firstArgument : otherArguments
  void $ skipWhiteSpaces *> string ")"
  return $ HiExprApply (HiExprValue function) $ fromMaybe [] arguments

parseExpression :: HiParser HiExpr
parseExpression =
  (HiExprValue <$> parseNumeric) <|>
  parseApplication

skipWhiteSpaces :: HiParser ()
skipWhiteSpaces = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")
