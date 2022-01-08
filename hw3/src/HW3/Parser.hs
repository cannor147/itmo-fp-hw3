module HW3.Parser
  ( parse
  ) where

import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Foldable                  (foldl')
import           Data.Maybe                     (fromMaybe)
import           Data.Void                      (Void)
import           HW3.Base
import           HW3.Lexer
import           Text.Megaparsec                (Parsec, many, optional, (<|>))
import           Text.Megaparsec.Error          (ParseErrorBundle (..))

type HiParser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = parseFully parseExpression

parseExpression :: HiParser HiExpr
parseExpression = parseOperators

parseApplication :: HiParser HiExpr
parseApplication = do
  head' <- parseValue <|> parseBrackets
  tail' <- many $ do
    void open
    arguments <- optional $ do
      firstArgument  <- parseOperators
      otherArguments <- many $ comma *> parseOperators
      return $ firstArgument : otherArguments
    void close
    return $ \applier -> HiExprApply applier $ fromMaybe [] arguments
  return $ foldl' (flip id) head' tail'

parseOperators :: HiParser HiExpr
parseOperators = makeExprParser (parseApplication <|> parseBrackets)
  [ [ binaryOperator InfixL HiFunMul asterisk
    , binaryOperator InfixL HiFunDiv slash
    ]
  , [ binaryOperator InfixL HiFunAdd plus
    , binaryOperator InfixL HiFunSub minus
    ]
  , [ binaryOperator InfixN HiFunEquals         eq
    , binaryOperator InfixN HiFunNotEquals      neq
    , binaryOperator InfixN HiFunGreaterThan    ge
    , binaryOperator InfixN HiFunLessThan       le
    , binaryOperator InfixN HiFunNotLessThan    gt
    , binaryOperator InfixN HiFunNotGreaterThan lt
    ]
  , [ binaryOperator InfixL HiFunAnd boolAnd
    ]
  , [ binaryOperator InfixL HiFunOr boolOr
    ]
  ]
  where
    binaryOperator operatorType function = operatorType . ((\a b -> apply function [a, b]) <$)

parseBrackets :: HiParser HiExpr
parseBrackets = open *> parseOperators <* close

parseValue :: HiParser HiExpr
parseValue = fmap HiExprValue $ parseFunction <|> parseNumeric <|> parseBool

parseFunction :: HiParser HiValue
parseFunction = fmap HiValueFunction $
  (keyword "add"              >> return HiFunAdd)            <|>
  (keyword "sub"              >> return HiFunSub)            <|>
  (keyword "mul"              >> return HiFunMul)            <|>
  (keyword "div"              >> return HiFunDiv)            <|>
  (keyword "not"              >> return HiFunNot)            <|>
  (keyword "and"              >> return HiFunAnd)            <|>
  (keyword "or"               >> return HiFunOr)             <|>
  (keyword "equals"           >> return HiFunEquals)         <|>
  (keyword "less-than"        >> return HiFunLessThan)       <|>
  (keyword "greater-than"     >> return HiFunGreaterThan)    <|>
  (keyword "not-equals"       >> return HiFunNotEquals)      <|>
  (keyword "not-less-than"    >> return HiFunNotLessThan)    <|>
  (keyword "not-greater-than" >> return HiFunNotGreaterThan) <|>
  (keyword "if"               >> return HiFunIf)

parseNumeric :: HiParser HiValue
parseNumeric = HiValueNumber <$> number

parseBool :: HiParser HiValue
parseBool = fmap HiValueBool $ (keyword "true" >> return True) <|> (keyword "false" >> return False)
