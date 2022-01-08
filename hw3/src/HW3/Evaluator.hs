{-# LANGUAGE FlexibleInstances #-}

module HW3.Evaluator
  ( eval
  ) where

import           HW3.Base

eval :: Monad m => HiExpr -> m EvaluationResult
eval (HiExprApply (HiExprValue (HiValueFunction fun)) args) = evalFunction fun args
eval (HiExprApply (HiExprValue (HiValueNumber _)) _)        = pure $ Left HiErrorInvalidFunction
eval (HiExprApply (HiExprApply _ _) _)                      = pure $ Left HiErrorInvalidFunction
eval (HiExprValue value)                                    = pure $ Right value

evalFunction :: Monad m => HiFun -> [HiExpr] -> m EvaluationResult
evalFunction HiFunAdd [first, second] = evaluateBinaryFunction (+) first second
evalFunction HiFunSub [first, second] = evaluateBinaryFunction (-) first second
evalFunction HiFunMul [first, second] = evaluateBinaryFunction (*) first second
evalFunction HiFunDiv [first, second] = evaluateBinaryFunction (/) first second
evalFunction _        _               = pure $ Left HiErrorArityMismatch

evaluateBinaryFunction :: Monad m => BinaryOperator -> HiExpr -> HiExpr -> m EvaluationResult
evaluateBinaryFunction operation firstArgument secondArgument = do
  firstValue  <- eval firstArgument
  secondValue <- eval secondArgument
  return $ operation firstValue secondValue
