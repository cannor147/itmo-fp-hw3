{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HW3.Evaluator
  ( eval
  , evaluate
  ) where

import           Data.Text   (Text)
import           GHC.Real    (Ratio (..))
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

er :: Value -> Value
er hiError@(Er _) = hiError
er _              = Er HiErrorInvalidArgument

er2 :: Value -> Value -> Value
er2 hiError@(Er _) _ = hiError
er2 _              a = er a

er3 :: Value -> Value -> Value -> Value
er3 hiError@(Er _) _ _ = hiError
er3 _              a b = er2 a b

boolean :: (Bool -> Bool) -> UnaryOperator
boolean op (Val (HiValueBool a)) = Val $ HiValueBool $ op a
boolean _  a                     = er a

arithmetic :: (Rational -> Rational) -> UnaryOperator
arithmetic op (Val (HiValueNumber a)) = Val $ HiValueNumber $ op a
arithmetic _  a                       = er a

string :: (Text -> Text) -> UnaryOperator
string op (Val (HiValueString a)) = Val $ HiValueString $ op a
string _  a                       = er a

boolean2 :: (Bool -> Bool -> Bool) -> BinaryOperator
boolean2 op (Val (HiValueBool a)) (Val (HiValueBool b)) = Val $ HiValueBool $ op a b
boolean2 _  a                     b                     = er2 a b

arithmetic2 :: (Rational -> Rational -> Rational) -> BinaryOperator
arithmetic2 op (Val (HiValueNumber a)) (Val (HiValueNumber b)) = Val $ HiValueNumber $ op a b
arithmetic2 _  a                       b                       = er2 a b

instance Num Value where
  abs                                                     = arithmetic abs
  signum                                                  = arithmetic signum
  (+) a@(Val (HiValueString _)) b@(Val (HiValueString _)) = (~+~) a b
  (+) a                         b                         = arithmetic2 (+) a b
  (-)                                                     = arithmetic2 (-)
  (*) a@(Val (HiValueString _)) b@(Val (HiValueNumber _)) = (~*~) a b
  (*) a                         b                         = arithmetic2 (*) a b
  fromInteger                                             = Val . HiValueNumber . fromInteger

instance Fractional Value where
  (/) a@(Val (HiValueString _)) b@(Val (HiValueString _)) = (~/~) a b
  (/) _                         (Val (HiValueNumber 0))   = Er HiErrorDivideByZero
  (/) a                         b                         = arithmetic2 (/) a b
  fromRational                                            = Val . HiValueNumber

instance Boolean Value where
  (#&&#)   = boolean2 (&&)
  (#||#)   = boolean2 (||)
  (#!#)    = boolean not
  fromBool = Val . HiValueBool

instance Equalable Value Value where
  (#==#) (Val (HiValueFunction a)) (Val (HiValueFunction b)) = fromBool $ a == b
  (#==#) (Val (HiValueNumber a))   (Val (HiValueNumber b))   = fromBool $ a == b
  (#==#) (Val (HiValueBool a))     (Val (HiValueBool b))     = fromBool $ a == b
  (#==#) (Val (HiValueString a))   (Val (HiValueString b))   = fromBool $ a == b
  (#==#) hiError@(Er _)            _                         = hiError
  (#==#) _                         hiError@(Er _)            = hiError
  (#==#) _                         _                         = fromBool False

instance Comparable Value Value where
  (#<=#) (Val (HiValueFunction a)) (Val (HiValueFunction b)) = fromBool $ a <= b
  (#<=#) (Val (HiValueNumber a))   (Val (HiValueNumber b))   = fromBool $ a <= b
  (#<=#) (Val (HiValueNumber _))   (Val (HiValueBool _))     = fromBool False
  (#<=#) (Val (HiValueBool a))     (Val (HiValueBool b))     = fromBool $ a <= b
  (#<=#) (Val (HiValueBool _))     (Val (HiValueNumber _))   = fromBool True
  (#<=#) (Val (HiValueString a))   (Val (HiValueString b))   = fromBool $ a <= b
  (#<=#) a                         b                         = er2 a b

instance Conditional Value Value where
  (#?:#) (Val (HiValueBool True))  value _     = value
  (#?:#) (Val (HiValueBool False)) _     value = value
  (#?:#) a                         _     _     = er a

instance Iterable Value where
  (~<~) (Val (HiValueString a)) = fromText $ (~<~) a
  (~<~) a                       = er a
  (~+~) (Val (HiValueString a)) (Val (HiValueString b)) = fromText $ (~+~) a b
  (~+~) a                       b                       = er2 a b

instance Multiplable Value Value where
  (~*~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = fromText $ (~*~) a b
  (~*~) a                       b                              = er2 a b

instance Indexable Value Value where
  (~#~) (Val (HiValueString a)) = fromInteger $ (~#~) a
  (~#~) a                       = er a
  (~$~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = fromText $ (~$~) a b
  (~$~) a                       b                              = er2 a b
  (~|~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) (Val (HiValueNumber (c :% 1))) =
    fromText $ (~|~) a b c
  (~|~) a                       b                              c                              =
    er3 a b c

instance Stringable Value where
  (~^~) = string (~^~)
  (~%~) = string (~%~)
  (~~~) = string (~~~)
  fromText text = Val $ HiValueString text

evaluate :: Monad m => HiExpr -> m Value
evaluate (HiExprValue value)                                    = pure $ Val value
evaluate (HiExprApply (HiExprValue (HiValueFunction fun)) args) = evaluateFunction fun args
evaluate (HiExprApply str@(HiExprValue (HiValueString _)) args) = evaluateSlicing str args
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
evaluateFunction HiFunLength         [x]       = evaluateUnaryFunction   (~#~)  x
evaluateFunction HiFunToUpper        [x]       = evaluateUnaryFunction   (~^~)  x
evaluateFunction HiFunToLower        [x]       = evaluateUnaryFunction   (~%~)  x
evaluateFunction HiFunReverse        [x]       = evaluateUnaryFunction   (~<~)  x
evaluateFunction HiFunTrim           [x]       = evaluateUnaryFunction   (~~~)  x
evaluateFunction _                   _         = pure $ Er HiErrorArityMismatch

evaluateSlicing :: Monad m => HiExpr -> [HiExpr] -> m Value
evaluateSlicing text [x]    = evaluateBinaryFunction (~$~) text x
evaluateSlicing text [x, y] = evaluateTernaryFunction (~|~) text x y
evaluateSlicing _    _      = pure $ Er HiErrorArityMismatch

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
