{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}

module HW3.Base
  ( HiAction(..)
  , HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiMonad(..)
  , HiValue(..)
  , apply
  ) where

import           Codec.Serialise   (Serialise)
import           Data.ByteString   (ByteString)
import           Data.Map.Internal (Map)
import           Data.Sequence     (Seq)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

-- | Hi function.
data HiFun =
    HiFunDiv
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
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Hi action.
data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Hi primitives.
data HiValue =
    HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueDict (Map HiValue HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueNull
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Hi expressions.
data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprDict [(HiExpr, HiExpr)]
  | HiExprRun HiExpr
  deriving (Show, Eq)

-- | Hi errors.
data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  | HiErrorInvalidState
  deriving (Show, Eq)

-- | Hi monad.
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

-- | Applies function to arguments.
apply :: HiFun -> [HiExpr] -> HiExpr
apply = HiExprApply . HiExprValue . HiValueFunction
