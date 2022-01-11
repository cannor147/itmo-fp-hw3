{-# LANGUAGE LambdaCase            #-}

module HW3.Evaluator
  ( eval
  , evaluate
  ) where

import           Control.Applicative (liftA2)
import           Data.Foldable       (foldl')
import           Data.Map            (empty, singleton)
import           HW3.Base
import           HW3.Classes
import           HW3.Value

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expression = do
  evaluationResult <- evaluate expression
  pure $ case evaluationResult of
    Er  hiError -> Left hiError
    Val hiValue -> Right hiValue

evaluate :: HiMonad m => HiExpr -> m Value
evaluate (HiExprValue value)                                     = pure $ Val value
evaluate (HiExprDict keyValues)                                  = evaluateDict keyValues
evaluate (HiExprApply (HiExprValue (HiValueFunction fun)) args)  = evaluateFunction fun args
evaluate (HiExprApply str@(HiExprValue (HiValueString _)) args)  = evaluateSlicing str args
evaluate (HiExprApply list@(HiExprValue (HiValueList _)) args)   = evaluateSlicing list args
evaluate (HiExprApply dict@(HiExprValue (HiValueDict _)) args)   = evaluateSlicing dict args
evaluate (HiExprApply bytes@(HiExprValue (HiValueBytes _)) args) = evaluateSlicing bytes args
evaluate (HiExprApply (HiExprValue _) _)                         = pure $ Er HiErrorInvalidFunction
evaluate (HiExprApply expression arguments)                      = evaluate expression >>= \case
  Val hiValue -> evaluate $ HiExprApply (HiExprValue hiValue) arguments
  other       -> pure other
evaluate (HiExprRun (HiExprValue (HiValueAction action)))        = Val <$> runAction action
evaluate (HiExprRun expression)                                  = evaluate expression >>= \case
  Val hiValue -> evaluate $ HiExprRun $ HiExprValue hiValue
  other       -> pure other

evaluateFunction :: HiMonad m => HiFun -> [HiExpr] -> m Value
evaluateFunction HiFunAdd            [x, y]    = evaluateBinaryFunction  (+)    x y
evaluateFunction HiFunSub            [x, y]    = evaluateBinaryFunction  (-)    x y
evaluateFunction HiFunMul            [x, y]    = evaluateBinaryFunction  (*)    x y
evaluateFunction HiFunDiv            [x, y]    = evaluateBinaryFunction  (/)    x y
evaluateFunction HiFunNot            [x]       = evaluateUnaryFunction   (#!#)  x
evaluateFunction HiFunAnd            [x, y]    = evaluateBinaryFunction  (#&&#) x y
evaluateFunction HiFunOr             [x, y]    = evaluateBinaryFunction  (#||#) x y
evaluateFunction HiFunLessThan       [x, y]    = evaluateBinaryFunction  (#<=#) x y
evaluateFunction HiFunGreaterThan    [x, y]    = evaluateBinaryFunction  (#>=#) x y
evaluateFunction HiFunEquals         [x, y]    = evaluateBinaryFunction  (#==#) x y
evaluateFunction HiFunNotLessThan    [x, y]    = evaluateBinaryFunction  (#>#)  x y
evaluateFunction HiFunNotGreaterThan [x, y]    = evaluateBinaryFunction  (#<#)  x y
evaluateFunction HiFunNotEquals      [x, y]    = evaluateBinaryFunction  (#!=#) x y
evaluateFunction HiFunIf             [x, y, z] = evaluateTernaryFunction (#?:#) x y z
evaluateFunction HiFunLength         [x]       = evaluateUnaryFunction   (~#~)  x
evaluateFunction HiFunToUpper        [x]       = evaluateUnaryFunction   (~|~)  x
evaluateFunction HiFunToLower        [x]       = evaluateUnaryFunction   (~.~)  x
evaluateFunction HiFunReverse        [x]       = evaluateUnaryFunction   (~<~)  x
evaluateFunction HiFunTrim           [x]       = evaluateUnaryFunction   (~-~)  x
evaluateFunction HiFunList           elements  = evaluateList                   elements
evaluateFunction HiFunRange          [x, y]    = evaluateBinaryFunction  range  x y
evaluateFunction HiFunFold           [x, y]    = do
  a <- evaluate x
  b <- evaluate y
  fold evaluateFunction a b
evaluateFunction HiFunCount          [x]       = evaluateUnaryFunction   (.#.)  x
evaluateFunction HiFunKeys           [x]       = evaluateUnaryFunction   (.^.)  x
evaluateFunction HiFunValues         [x]       = evaluateUnaryFunction   (.$.)  x
evaluateFunction HiFunInvert         [x]       = evaluateUnaryFunction   (.!.)  x
evaluateFunction HiFunPackBytes      [x]       = evaluateUnaryFunction   listToBytes x
evaluateFunction HiFunUnpackBytes    [x]       = evaluateUnaryFunction   bytesToList x
evaluateFunction HiFunEncodeUtf8     [x]       = evaluateUnaryFunction   fromUtf8 x
evaluateFunction HiFunDecodeUtf8     [x]       = evaluateUnaryFunction   toUtf8 x
evaluateFunction HiFunZip            [x]       = evaluateUnaryFunction   zip' x
evaluateFunction HiFunUnzip          [x]       = evaluateUnaryFunction   unzip' x
evaluateFunction HiFunSerialise      [x]       = evaluateUnaryFunction   valueToBytes x
evaluateFunction HiFunDeserialise    [x]       = evaluateUnaryFunction   bytesToValue x
evaluateFunction HiFunRead           [x]       = evaluateUnaryFunction   read' x
evaluateFunction HiFunWrite          [x, y]    = evaluateBinaryFunction  write' x y
evaluateFunction HiFunMkDir          [x]       = evaluateUnaryFunction   mkdir' x
evaluateFunction HiFunChDir          [x]       = evaluateUnaryFunction   cd' x
evaluateFunction _                   _         = pure $ Er HiErrorArityMismatch

evaluateSlicing :: HiMonad m => HiExpr -> [HiExpr] -> m Value
evaluateSlicing sliceable [x]    = evaluateBinaryFunction  (~@~) sliceable x
evaluateSlicing sliceable [x, y] = evaluateTernaryFunction (~!~) sliceable x y
evaluateSlicing _         _      = pure $ Er HiErrorArityMismatch

evaluateUnaryFunction :: HiMonad m => UnaryOperator -> HiExpr -> m Value
evaluateUnaryFunction operation firstArgument = do
  firstValue <- evaluate firstArgument
  return $ operation firstValue

evaluateBinaryFunction :: HiMonad m => BinaryOperator -> HiExpr -> HiExpr -> m Value
evaluateBinaryFunction operation firstArgument secondArgument = do
  firstValue  <- evaluate firstArgument
  secondValue <- evaluate secondArgument
  return $ operation firstValue secondValue

evaluateTernaryFunction :: HiMonad m => TernaryOperator -> HiExpr -> HiExpr -> HiExpr -> m Value
evaluateTernaryFunction operation firstArgument secondArgument thirdArgument = do
  firstValue  <- evaluate firstArgument
  secondValue <- evaluate secondArgument
  thirdValue  <- evaluate thirdArgument
  return $ operation firstValue secondValue thirdValue

evaluateList :: HiMonad m => [HiExpr] -> m Value
evaluateList elements = foldl' (liftA2 (~&~)) (pure (~~~)) $ map evaluate elements

evaluateDict :: HiMonad m => [(HiExpr, HiExpr)] -> m Value
evaluateDict dict = foldl' (liftA2 (.+.)) emptyDict $ map evaluateKeyValue dict
  where
    emptyDict = pure . fromDict $ Data.Map.empty
    evaluateKeyValue (key, value) = do
      evaluatedKey   <- evaluate key
      evaluatedValue <- evaluate value
      case (evaluatedKey, evaluatedValue) of
        (Val a, Val b) -> pure . fromDict $ Data.Map.singleton a b
        (a    , b)     -> pure $ er2 a b
