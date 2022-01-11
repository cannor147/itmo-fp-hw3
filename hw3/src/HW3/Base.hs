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
import Data.Map.Internal (Map)

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
           | HiFunCount
           | HiFunKeys
           | HiFunValues
           | HiFunInvert
  deriving (Show, Eq, Ord)

data HiValue = HiValueNumber Rational
             | HiValueFunction HiFun
             | HiValueBool Bool
             | HiValueNull
             | HiValueString Text
             | HiValueList (Seq HiValue)
             | HiValueDict (Map HiValue HiValue)
  deriving Show

data HiExpr = HiExprValue HiValue
            | HiExprApply HiExpr [HiExpr]
            | HiExprDict [(HiExpr, HiExpr)]
  deriving Show

data HiError = HiErrorInvalidArgument
             | HiErrorInvalidFunction
             | HiErrorArityMismatch
             | HiErrorDivideByZero
             | HiErrorInvalidState
  deriving Show

apply :: HiFun -> [HiExpr] -> HiExpr
apply = HiExprApply . HiExprValue . HiValueFunction

cmp :: HiValue -> HiValue -> Ordering
cmp (HiValueFunction a) (HiValueFunction b) = compare a b
cmp (HiValueNumber _)   (HiValueFunction _) = LT
cmp (HiValueNumber a)   (HiValueNumber b)   = compare a b
cmp (HiValueBool _)     (HiValueFunction _) = LT
cmp (HiValueBool _)     (HiValueNumber _)   = LT
cmp (HiValueBool a)     (HiValueBool b)     = compare a b
cmp (HiValueString _)   (HiValueFunction _) = LT
cmp (HiValueString _)   (HiValueNumber _)   = LT
cmp (HiValueString _)   (HiValueBool _)     = LT
cmp (HiValueString a)   (HiValueString b)   = compare a b
cmp (HiValueList _)     (HiValueFunction _) = LT
cmp (HiValueList _)     (HiValueNumber _)   = LT
cmp (HiValueList _)     (HiValueBool _)     = LT
cmp (HiValueList _)     (HiValueString _)   = LT
cmp (HiValueList a)     (HiValueList b)     = compare a b
cmp (HiValueDict _)     (HiValueFunction _) = LT
cmp (HiValueDict _)     (HiValueNumber _)   = LT
cmp (HiValueDict _)     (HiValueBool _)     = LT
cmp (HiValueDict _)     (HiValueString _)   = LT
cmp (HiValueDict _)     (HiValueList _)     = LT
cmp (HiValueDict a)     (HiValueDict b)     = compare a b
cmp _                   _                   = GT

instance Eq HiValue where
  (==) a b = (==) EQ $ cmp a b

instance Ord HiValue where
  compare = cmp
