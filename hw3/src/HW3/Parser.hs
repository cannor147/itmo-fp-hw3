{-# LANGUAGE BlockArguments #-}

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
import Data.Text (pack)

type HiParser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = parseFully parseExpression

parseExpression :: HiParser HiExpr
parseExpression = makeExprParser (parseApplication <|> parseBrackets)
  [ [ InfixL ((\a b -> HiExprApply a [b]) <$ dot)
    ]
  , [ binaryOperator InfixL HiFunMul asterisk
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

parseApplication :: HiParser HiExpr
parseApplication = do
  head' <- parseUnit <|> parseBrackets
  tail' <- many $ application <|> field
  return $ foldl' (flip id) head' tail'
    where
    application = do
      void open
      arguments <- optional $ do
        firstArgument  <- parseExpression
        otherArguments <- many $ comma *> parseExpression
        return $ firstArgument : otherArguments
      void close
      return $ \applier -> HiExprApply applier $ fromMaybe [] arguments
    field = do
      fieldName <- dot *> word
      return $ \applier -> HiExprApply applier [HiExprValue $ HiValueString $ pack fieldName]

parseBrackets :: HiParser HiExpr
parseBrackets = open *> parseExpression <* close

parseUnit :: HiParser HiExpr
parseUnit = parseValue <|> parseList <|> parseDictionary

parseValue :: HiParser HiExpr
parseValue = fmap HiExprValue $
  parseFunction <|>
  parseNumeric  <|>
  parseBool     <|>
  parseString   <|>
  parseNull

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
  (keyword "if"               >> return HiFunIf)             <|>
  (keyword "length"           >> return HiFunLength)         <|>
  (keyword "to-upper"         >> return HiFunToUpper)        <|>
  (keyword "to-lower"         >> return HiFunToLower)        <|>
  (keyword "reverse"          >> return HiFunReverse)        <|>
  (keyword "trim"             >> return HiFunTrim)           <|>
  (keyword "list"             >> return HiFunList)           <|>
  (keyword "range"            >> return HiFunRange)          <|>
  (keyword "fold"             >> return HiFunFold)           <|>
  (keyword "count"            >> return HiFunCount)          <|>
  (keyword "keys"             >> return HiFunKeys)           <|>
  (keyword "values"           >> return HiFunValues)         <|>
  (keyword "invert"           >> return HiFunInvert)

parseNumeric :: HiParser HiValue
parseNumeric = fmap HiValueNumber number

parseBool :: HiParser HiValue
parseBool = fmap HiValueBool $ (keyword "true" >> return True) <|> (keyword "false" >> return False)

parseString :: HiParser HiValue
parseString = fmap HiValueString string

parseNull :: HiParser HiValue
parseNull = keyword "null" >> return HiValueNull

parseList :: HiParser HiExpr
parseList = fmap (HiExprApply $ HiExprValue $ HiValueFunction HiFunList) do
  void openSquare
  arguments <- optional $ do
    firstArgument  <- parseExpression
    otherArguments <- many $ comma *> parseExpression
    return $ firstArgument : otherArguments
  void closeSquare
  return $ fromMaybe mempty arguments

parseDictionary :: HiParser HiExpr
parseDictionary = fmap HiExprDict do
  void openBrace
  arguments <- optional $ do
    firstKeyValue  <- parseKeyValue
    otherKeyValues <- many $ comma *> parseKeyValue
    return $ firstKeyValue : otherKeyValues
  void closeBrace
  return $ fromMaybe mempty arguments
    where
      parseKeyValue = do
        key <- parseExpression
        value <- colon *> parseExpression
        return (key, value)
