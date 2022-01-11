{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HW3.Evaluator
  ( eval
  , evaluate
  ) where

import           Control.Applicative (liftA2)
import           Data.Foldable       (foldl', toList)
import           Data.Map            (Map, empty, fromListWith, keys, singleton,
                                      toList, lookup)
import           Data.Sequence       (Seq (..), fromList)
import           Data.Text           (Text, unpack)
import           GHC.Real            (Ratio (..))
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

fromChar :: Char -> Value
fromChar = fromText . fromElement

fromSeq :: Seq HiValue -> Value
fromSeq = Val . HiValueList

fromDict :: Map HiValue HiValue -> Value
fromDict = Val . HiValueDict

hiNull :: Value
hiNull = Val HiValueNull

instance Num Value where
  abs                                                     = arithmetic abs
  signum                                                  = arithmetic signum
  (+) a@(Val (HiValueString _)) b@(Val (HiValueString _)) = (~+~) a b
  (+) a@(Val (HiValueList _))   b@(Val (HiValueList _))   = (~+~) a b
  (+) a                         b                         = arithmetic2 (+) a b
  (-)                                                     = arithmetic2 (-)
  (*) a@(Val (HiValueString _)) b@(Val (HiValueNumber _)) = (~*~) a b
  (*) a@(Val (HiValueList _))   b@(Val (HiValueNumber _)) = (~*~) a b
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
  (#==#) (Val a) (Val b) = fromBool $ a == b
  (#==#) a       b       = er2 a b

instance Comparable Value Value where
  (#<=#) (Val a) (Val b) = fromBool $ a <= b
  (#<=#) a       b       = er2 a b

instance Conditional Value Value where
  (#?:#) (Val (HiValueBool True))  value _     = value
  (#?:#) (Val (HiValueBool False)) _     value = value
  (#?:#) a                         _     _     = er a

instance Sequenceable Value where
  (~~~)                                                 = fromSeq (~~~)
  (~<~) (Val (HiValueString a)) = fromText $ (~<~) a
  (~<~) (Val (HiValueList a))   = fromSeq $ (~<~) a
  (~<~) a                       = er a
  (~+~) (Val (HiValueString a)) (Val (HiValueString b)) = fromText $ (~+~) a b
  (~+~) (Val (HiValueList a))   (Val (HiValueList b))   = fromSeq $ (~+~) a b
  (~+~) a                       b                       = er2 a b

instance Sliceable Value Value where
  (~#~) (Val (HiValueString a)) = fromInteger $ (~#~) a
  (~#~) (Val (HiValueList a))   = fromInteger $ (~#~) a
  (~#~) a                       = er a
  (~*~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = fromText $ (~*~) a b
  (~*~) (Val (HiValueList a))   (Val (HiValueNumber (b :% 1))) = fromSeq $ (~*~) a b
  (~*~) a                       b                              = er2 a b
  (~^~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = fromText $ (~^~) a b
  (~^~) (Val (HiValueList a))   (Val (HiValueNumber (b :% 1))) = fromSeq $ (~^~) a b
  (~^~) a                       b                              = er2 a b
  (~$~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = fromText $ (~$~) a b
  (~$~) (Val (HiValueList a))   (Val (HiValueNumber (b :% 1))) = fromSeq $ (~$~) a b
  (~$~) a                       b                              = er2 a b

instance Contentable Value Value where
  (~&~)       (Val (HiValueList a)) (Val b) = fromSeq $ (~&~) a b
  (~&~)       a                     b       = er2 a b
  fromElement (Val a)        = fromSeq $ fromElement a
  fromElement hiError@(Er _) = hiError

instance Iterable Value Value Value where
  (~@~) (Val (HiValueDict a))   (Val b)                        = maybe hiNull Val foundValue
    where
      foundValue = Data.Map.lookup b a
  (~@~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = fromChar ((~@~) a b :: Char)
  (~@~) (Val (HiValueList a))   (Val (HiValueNumber (b :% 1))) = Val $ (~@~) a b
  (~@~) a                       b                              = er2 a b

instance Stringable Value where
  (~|~) = string (~|~)
  (~.~) = string (~.~)
  (~-~) = string (~-~)
  fromText text = Val $ HiValueString text

(.+.) :: Value -> Value -> Value
(.+.) (Val (HiValueDict a)) (Val (HiValueDict b)) = fromDict $ (<>) a b
(.+.) a                     b                     = er2 a b

(.^.) :: Value -> Value
(.^.) (Val (HiValueDict a)) = fromSeq . fromList $ keys a
(.^.) a                     = er a

(.$.) :: Value -> Value
(.$.) (Val (HiValueDict a)) = fromSeq . fromList $ map snd $ Data.Map.toList a
(.$.) a                     = er a

toCountMap :: (Ord k, Foldable t) => t k -> Map k HiValue
toCountMap a = HiValueNumber <$> fromListWith (+) (flip zip (repeat 1) $ Data.Foldable.toList a)

(.#.) :: Value -> Value
(.#.) (Val (HiValueList a))   = fromDict $ toCountMap a
(.#.) (Val (HiValueString a)) = fromDict $ toCountMap $ HiValueString . fromElement <$> unpack a
(.#.) a                     = er a

(.!.) :: Value -> Value
(.!.) (Val (HiValueDict a)) = fromDict $ fmap (HiValueList . fromList) inverseMap
  where
    inverseMap = fromListWith (<>) $ (\(x, y) -> (y, [x])) <$> Data.Map.toList a
(.!.) a                     = er a

range :: Value -> Value -> Value
range (Val (HiValueNumber a)) (Val (HiValueNumber b)) = fromSeq $ fromList $ generateRange a b
  where
    generateRange x y = fmap HiValueNumber [x..y]
range a                       b                       = er2 a b

fold :: Monad m => Value -> Value -> m Value
fold (Val (HiValueFunction _)) (Val (HiValueList Empty))    = pure $ Val HiValueNull
fold (Val (HiValueFunction f)) (Val (HiValueList elements)) = do
  let values = fmap (pure . Val) elements
  flip foldl1 values $ \a b -> a >>= \x -> b >>= \y -> case (x, y) of
    (Val i, Val j) -> evaluateFunction f [HiExprValue i, HiExprValue j]
    (i, j)         -> pure $ er2 i j
fold a                          b                           = pure $ er2 a b

evaluate :: Monad m => HiExpr -> m Value
evaluate (HiExprValue value)                                    = pure $ Val value
evaluate (HiExprDict keyValues)                                 = evaluateDict keyValues
evaluate (HiExprApply (HiExprValue (HiValueFunction fun)) args) = evaluateFunction fun args
evaluate (HiExprApply str@(HiExprValue (HiValueString _)) args) = evaluateSlicing str args
evaluate (HiExprApply list@(HiExprValue (HiValueList _)) args)  = evaluateSlicing list args
evaluate (HiExprApply dict@(HiExprValue (HiValueDict _)) args)  = evaluateSlicing dict args
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
evaluateFunction HiFunToUpper        [x]       = evaluateUnaryFunction   (~|~)  x
evaluateFunction HiFunToLower        [x]       = evaluateUnaryFunction   (~.~)  x
evaluateFunction HiFunReverse        [x]       = evaluateUnaryFunction   (~<~)  x
evaluateFunction HiFunTrim           [x]       = evaluateUnaryFunction   (~-~)  x
evaluateFunction HiFunList           elements  = evaluateList                   elements
evaluateFunction HiFunRange          [x, y]    = evaluateBinaryFunction  range  x y
evaluateFunction HiFunFold           [x, y]    = evaluate x >>= \a -> evaluate y >>= \b -> fold a b
evaluateFunction HiFunCount          [x]       = evaluateUnaryFunction   (.#.)  x
evaluateFunction HiFunKeys           [x]       = evaluateUnaryFunction   (.^.)  x
evaluateFunction HiFunValues         [x]       = evaluateUnaryFunction   (.$.)  x
evaluateFunction HiFunInvert         [x]       = evaluateUnaryFunction   (.!.)  x
evaluateFunction _                   _         = pure $ Er HiErrorArityMismatch

evaluateSlicing :: Monad m => HiExpr -> [HiExpr] -> m Value
evaluateSlicing sliceable [x]    = evaluateBinaryFunction  (~@~) sliceable x
evaluateSlicing sliceable [x, y] = evaluateTernaryFunction (~!~) sliceable x y
evaluateSlicing _         _      = pure $ Er HiErrorArityMismatch

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

evaluateList :: Monad m => [HiExpr] -> m Value
evaluateList elements = foldl' (liftA2 (~&~)) (pure (~~~)) $ map evaluate elements

evaluateDict :: Monad m => [(HiExpr, HiExpr)] -> m Value
evaluateDict dict = foldl' (liftA2 (.+.)) emptyDict $ map evaluateKeyValue dict
  where
    emptyDict = pure . fromDict $ Data.Map.empty
    evaluateKeyValue (key, value) = do
      evaluatedKey   <- evaluate key
      evaluatedValue <- evaluate value
      case (evaluatedKey, evaluatedValue) of
        (Val a, Val b) -> pure . fromDict $ Data.Map.singleton a b
        (a    , b)     -> pure $ er2 a b
