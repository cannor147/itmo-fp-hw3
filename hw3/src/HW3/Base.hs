{-# LANGUAGE FlexibleInstances #-}

module HW3.Base
  ( HiFun(..)
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  , apply
  ) where

import           Data.Sequence (Seq)
import           Data.Text     (Text)

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
           | HiFunList
           | HiFunRange
           | HiFunFold
  deriving (Show, Eq, Ord)

data HiValue = HiValueNumber Rational
             | HiValueFunction HiFun
             | HiValueBool Bool
             | HiValueNull
             | HiValueString Text
             | HiValueList (Seq HiValue)
  deriving Show

data HiExpr = HiExprValue HiValue
            | HiExprApply HiExpr [HiExpr]
  deriving Show

data HiError = HiErrorInvalidArgument
             | HiErrorInvalidFunction
             | HiErrorArityMismatch
             | HiErrorDivideByZero
             | HiErrorInvalidState
  deriving Show

apply :: HiFun -> [HiExpr] -> HiExpr
apply = HiExprApply . HiExprValue . HiValueFunction
