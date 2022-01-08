{-# LANGUAGE FlexibleInstances #-}

module HW3.Base
  ( EvaluationResult
  , UnaryOperator
  , BinaryOperator
  , HiFun(..)
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  ) where

data HiFun = HiFunDiv
           | HiFunMul
           | HiFunAdd
           | HiFunSub
  deriving Show

data HiValue = HiValueNumber Rational
             | HiValueFunction HiFun
  deriving Show

data HiExpr = HiExprValue HiValue
            | HiExprApply HiExpr [HiExpr]
  deriving Show

data HiError = HiErrorInvalidArgument
             | HiErrorInvalidFunction
             | HiErrorArityMismatch
             | HiErrorDivideByZero
  deriving Show

type EvaluationResult = Either HiError HiValue
type UnaryOperator    = EvaluationResult -> EvaluationResult
type BinaryOperator   = EvaluationResult -> UnaryOperator

arithmetic :: (Rational -> Rational) -> UnaryOperator
arithmetic op (Right (HiValueNumber a)) = Right $ HiValueNumber $ op a
arithmetic _  hiError@(Left _)          = hiError
arithmetic _  _                         = Left HiErrorInvalidArgument

arithmetic2 :: (Rational -> Rational -> Rational) -> BinaryOperator
arithmetic2 op (Right (HiValueNumber a)) (Right (HiValueNumber b)) = Right $ HiValueNumber $ op a b
arithmetic2 _  hiError@(Left _)           _                        = hiError
arithmetic2 _  _                          hiError@(Left _)         = hiError
arithmetic2 _  _                          _                        = Left HiErrorInvalidArgument

instance Num EvaluationResult where
  abs         = arithmetic abs
  signum      = arithmetic signum
  (+)         = arithmetic2 (+)
  (-)         = arithmetic2 (-)
  (*)         = arithmetic2 (*)
  fromInteger = Right . HiValueNumber . fromInteger

instance Fractional EvaluationResult where
  (/) _ (Right (HiValueNumber 0)) = Left HiErrorDivideByZero
  (/) a b                         = arithmetic2 (/) a b
  fromRational                    = Right . HiValueNumber
