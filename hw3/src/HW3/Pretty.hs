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

prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction HiFunAdd = pretty "add"
prettyFunction HiFunSub = pretty "sub"
prettyFunction HiFunMul = pretty "mul"
prettyFunction HiFunDiv = pretty "div"

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber (n :% 1)        = pretty n
prettyNumber number@(n :% m) = case snd $ fromRationalRepetendUnlimited number of
  Nothing -> pretty (fromRational number :: Double)
  _       -> prettyFractional (quotRem n m)
    where
      prettyFractional (0, y) = pretty (show y <> "/" <> show m)
      prettyFractional (x, y) = pretty x <+> pretty (sign y) <+> prettyFractional (0, abs y)
      sign             x      = if x < 0 then "-" else "+"
