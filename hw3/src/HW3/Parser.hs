{-# LANGUAGE BlockArguments #-}

module HW3.Parser
  ( parse
  ) where

import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Foldable                  (foldl')
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (pack)
import           Data.Void                      (Void)
import           HW3.Base
import           HW3.Lexer
import           Text.Megaparsec                (Parsec, many, optional, (<|>), some)
import           Text.Megaparsec.Error          (ParseErrorBundle (..))
import Data.ByteString.Internal (packBytes)

type HiParser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = parseFully parseExpression

constructRuns :: (Eq t, Num t) => t -> HiExpr -> HiExpr
constructRuns 0 a = a
constructRuns n a = HiExprRun $ constructRuns (n - 1) a

parseExpression :: HiParser HiExpr
parseExpression = makeExprParser (parseApplication <|> parseBrackets)
  [ [ Postfix (constructRuns . length <$> some excl)
    ]
  , [ InfixL ((\a b -> HiExprApply a [b]) <$ dot)
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
  , [ binaryOperator InfixR HiFunAnd boolAnd
    ]
  , [ binaryOperator InfixR HiFunOr boolOr
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
parseUnit = parseValue <|> parseBytes <|> parseList <|> parseDictionary

parseValue :: HiParser HiExpr
parseValue = fmap HiExprValue $
  parseFunction <|>
  parseNumeric  <|>
  parseBool     <|>
  parseString   <|>
  parseAction   <|>
  parseNull

parseFunction :: HiParser HiValue
parseFunction = fmap HiValueFunction $
  (keyword "not-equals"       >> return HiFunNotEquals)      <|>
  (keyword "not-less-than"    >> return HiFunNotLessThan)    <|>
  (keyword "not-greater-than" >> return HiFunNotGreaterThan) <|>
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
  (keyword "invert"           >> return HiFunInvert)         <|>
  (keyword "pack-bytes"       >> return HiFunPackBytes)      <|>
  (keyword "unpack-bytes"     >> return HiFunUnpackBytes)    <|>
  (keyword "encode-utf8"      >> return HiFunEncodeUtf8)     <|>
  (keyword "decode-utf8"      >> return HiFunDecodeUtf8)     <|>
  (keyword "zip"              >> return HiFunZip)            <|>
  (keyword "unzip"            >> return HiFunUnzip)          <|>
  (keyword "serialise"        >> return HiFunSerialise)      <|>
  (keyword "deserialise"      >> return HiFunDeserialise)    <|>
  (keyword "read"             >> return HiFunRead)           <|>
  (keyword "write"            >> return HiFunWrite)          <|>
  (keyword "mkdir"            >> return HiFunMkDir)          <|>
  (keyword "cd"               >> return HiFunChDir)

parseNumeric :: HiParser HiValue
parseNumeric = fmap HiValueNumber number

parseBool :: HiParser HiValue
parseBool = fmap HiValueBool $ (keyword "true" >> return True) <|> (keyword "false" >> return False)

parseString :: HiParser HiValue
parseString = fmap HiValueString string

parseNull :: HiParser HiValue
parseNull = keyword "null" >> return HiValueNull

parseAction :: HiParser HiValue
parseAction = keyword "cwd" >> return (HiValueAction HiActionCwd)

parseList :: HiParser HiExpr
parseList = fmap (HiExprApply $ HiExprValue $ HiValueFunction HiFunList) do
  void openSquare
  arguments <- optional $ do
    firstArgument  <- parseExpression
    otherArguments <- many $ comma *> parseExpression
    return $ firstArgument : otherArguments
  void closeSquare
  return $ fromMaybe mempty arguments

parseBytes :: HiParser HiExpr
parseBytes = fmap (HiExprValue . HiValueBytes . packBytes) (openBytes *> many byte <* closeBytes)

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
