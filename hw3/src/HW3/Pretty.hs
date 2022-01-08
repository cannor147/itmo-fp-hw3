module HW3.Pretty
  ( prettyValue
  ) where

import           Data.Scientific               (fromRationalRepetendUnlimited)
import           GHC.Real                      (Ratio (..))
import           HW3.Base
import           Prettyprinter                 (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction function) = prettyFunction function
prettyValue (HiValueNumber number)     = prettyNumber number
prettyValue (HiValueBool bool)         = prettyBool bool

prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction HiFunAdd            = pretty "add"
prettyFunction HiFunSub            = pretty "sub"
prettyFunction HiFunMul            = pretty "mul"
prettyFunction HiFunDiv            = pretty "div"
prettyFunction HiFunNot            = pretty "not"
prettyFunction HiFunAnd            = pretty "and"
prettyFunction HiFunOr             = pretty "or"
prettyFunction HiFunLessThan       = pretty "equals"
prettyFunction HiFunGreaterThan    = pretty "less-than"
prettyFunction HiFunEquals         = pretty "greater-than"
prettyFunction HiFunNotLessThan    = pretty "not-equals"
prettyFunction HiFunNotGreaterThan = pretty "not-less-than"
prettyFunction HiFunNotEquals      = pretty "not-greater-than"
prettyFunction HiFunIf             = pretty "if"

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber (n :% 1)        = pretty n
prettyNumber number@(n :% m) = case snd $ fromRationalRepetendUnlimited number of
  Nothing -> pretty (fromRational number :: Double)
  _       -> prettyFractional (quotRem n m)
    where
      prettyFractional (0, y) = pretty (show y <> "/" <> show m)
      prettyFractional (x, y) = pretty x <+> pretty (sign y) <+> prettyFractional (0, abs y)
      sign             x      = if x < 0 then "-" else "+"

prettyBool :: Bool -> Doc AnsiStyle
prettyBool True  = pretty "true"
prettyBool False = pretty "false"
