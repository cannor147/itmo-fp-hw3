{-# LANGUAGE FlexibleInstances #-}

module HW3.Base
  ( HiFun(..)
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  , apply
  ) where

import           Data.Text (Text)

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
           | HiFunLength
           | HiFunToUpper
           | HiFunToLower
           | HiFunReverse
           | HiFunTrim
  deriving (Show, Eq, Ord)

data HiValue = HiValueNumber Rational
             | HiValueFunction HiFun
             | HiValueBool Bool
             | HiValueNull
             | HiValueString Text
  deriving Show

data HiExpr = HiExprValue HiValue
            | HiExprApply HiExpr [HiExpr]
  deriving Show

data HiError = HiErrorInvalidArgument
             | HiErrorInvalidFunction
             | HiErrorArityMismatch
             | HiErrorDivideByZero
  deriving Show

apply :: HiFun -> [HiExpr] -> HiExpr
apply = HiExprApply . HiExprValue . HiValueFunction
