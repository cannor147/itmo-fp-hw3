{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator
  ( eval
  , evaluate
  ) where

import           HW3.Base
import           HW3.Classes

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expression = do
  evaluationResult <- evaluate expression
  pure $ case evaluationResult of
    Er  hiError -> Left hiError
    Val hiValue -> Right hiValue

data Value = Er HiError | Val HiValue

type UnaryOperator   = Value -> Value
type BinaryOperator  = Value -> UnaryOperator
type TernaryOperator = Value -> BinaryOperator

boolean :: (Bool -> Bool) -> UnaryOperator
boolean op (Val (HiValueBool a)) = Val $ HiValueBool $ op a
boolean _  hiError@(Er _)        = hiError
boolean _  _                     = Er HiErrorInvalidArgument

arithmetic :: (Rational -> Rational) -> UnaryOperator
arithmetic op (Val (HiValueNumber a)) = Val $ HiValueNumber $ op a
arithmetic _  hiError@(Er _)          = hiError
arithmetic _  _                       = Er HiErrorInvalidArgument

boolean2 :: (Bool -> Bool -> Bool) -> BinaryOperator
boolean2 op (Val (HiValueBool a)) (Val (HiValueBool b)) = Val $ HiValueBool $ op a b
boolean2 _  hiError@(Er _)        _                     = hiError
boolean2 _  _                     hiError@(Er _)        = hiError
boolean2 _  _                     _                     = Er HiErrorInvalidArgument

arithmetic2 :: (Rational -> Rational -> Rational) -> BinaryOperator
arithmetic2 op (Val (HiValueNumber a)) (Val (HiValueNumber b)) = Val $ HiValueNumber $ op a b
arithmetic2 _  hiError@(Er _)           _                      = hiError
arithmetic2 _  _                        hiError@(Er _)         = hiError
arithmetic2 _  _                        _                      = Er HiErrorInvalidArgument

instance Num Value where
  abs         = arithmetic abs
  signum      = arithmetic signum
  (+)         = arithmetic2 (+)
  (-)         = arithmetic2 (-)
  (*)         = arithmetic2 (*)
  fromInteger = Val . HiValueNumber . fromInteger

instance Fractional Value where
  (/) _ (Val (HiValueNumber 0)) = Er HiErrorDivideByZero
  (/) a b                       = arithmetic2 (/) a b
  fromRational                  = Val . HiValueNumber

instance Boolean Value where
  (#&&#)   = boolean2 (&&)
  (#||#)   = boolean2 (||)
  (#!#)    = boolean not
  fromBool = Val . HiValueBool

instance Equalable Value Value where
  (#==#) (Val (HiValueFunction a)) (Val (HiValueFunction b)) = fromBool $ a == b
  (#==#) (Val (HiValueNumber a))   (Val (HiValueNumber b))   = fromBool $ a == b
  (#==#) (Val (HiValueBool a))     (Val (HiValueBool b))     = fromBool $ a == b
  (#==#) hiError@(Er _)            _                         = hiError
  (#==#) _                         hiError@(Er _)            = hiError
  (#==#) _                         _                         = fromBool False

instance Comparable Value Value where
  (#<=#) (Val (HiValueFunction a)) (Val (HiValueFunction b)) = fromBool $ a <= b
  (#<=#) (Val (HiValueFunction _)) _                         = fromBool False
  (#<=#) (Val (HiValueNumber _))   (Val (HiValueFunction _)) = fromBool True
  (#<=#) (Val (HiValueNumber a))   (Val (HiValueNumber b))   = fromBool $ a <= b
  (#<=#) (Val (HiValueNumber _))   (Val (HiValueBool _))     = fromBool False
  (#<=#) (Val (HiValueBool a))     (Val (HiValueBool b))     = fromBool $ a <= b
  (#<=#) (Val (HiValueBool _))     _                         = fromBool True
  (#<=#) hiError@(Er _)            _                         = hiError
  (#<=#) _                         hiError@(Er _)            = hiError

instance Conditional Value Value where
  (#?:#) hiError@(Er _)            _     _     = hiError
  (#?:#) (Val (HiValueBool True))  value _     = value
  (#?:#) (Val (HiValueBool False)) _     value = value
  (#?:#) _                         _     _     = Er HiErrorInvalidArgument

evaluate :: Monad m => HiExpr -> m Value
evaluate (HiExprValue value)                                    = pure $ Val value
evaluate (HiExprApply (HiExprValue (HiValueFunction fun)) args) = evaluateFunction fun args
evaluate (HiExprApply (HiExprValue _) _)                        = pure $ Er HiErrorInvalidFunction
evaluate (HiExprApply expression arguments)                     = evaluate expression >>= \case
  Val hiValue -> evaluate $ HiExprApply (HiExprValue hiValue) arguments
  other       -> pure other

evaluateFunction :: Monad m => HiFun -> [HiExpr] -> m Value
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
evaluateFunction _                   _      = pure $ Er HiErrorArityMismatch

evaluateUnaryFunction :: Monad m => UnaryOperator -> HiExpr -> m Value
evaluateUnaryFunction operation firstArgument = do
  firstValue <- evaluate firstArgument
  return $ operation firstValue

evaluateBinaryFunction :: Monad m => BinaryOperator -> HiExpr -> HiExpr -> m Value
evaluateBinaryFunction operation firstArgument secondArgument = do
  firstValue  <- evaluate firstArgument
  secondValue <- evaluate secondArgument
  return $ operation firstValue secondValue

evaluateTernaryFunction :: Monad m => TernaryOperator -> HiExpr -> HiExpr -> HiExpr -> m Value
evaluateTernaryFunction operation firstArgument secondArgument thirdArgument = do
  firstValue  <- evaluate firstArgument
  secondValue <- evaluate secondArgument
  thirdValue  <- evaluate thirdArgument
  return $ operation firstValue secondValue thirdValue
