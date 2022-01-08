{-# LANGUAGE FlexibleInstances #-}

module HW3.Base
  ( HiFun(..)
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  ) where

data HiFun = HiFunDiv
           | HiFunMul
           | HiFunAdd
           | HiFunSub
           | HiFunNot
           | HiFunAnd
           | HiFunOr
           | HiFunEquals
           | HiFunLessThan
           | HiFunGreaterThan
           | HiFunNotEquals
           | HiFunNotLessThan
           | HiFunNotGreaterThan
           | HiFunIf
  deriving (Show, Eq, Ord)

data HiValue = HiValueNumber Rational
             | HiValueFunction HiFun
             | HiValueBool Bool
  deriving Show

data HiExpr = HiExprValue HiValue
            | HiExprApply HiExpr [HiExpr]
  deriving Show

data HiError = HiErrorInvalidArgument
             | HiErrorInvalidFunction
             | HiErrorArityMismatch
             | HiErrorDivideByZero
  deriving Show
