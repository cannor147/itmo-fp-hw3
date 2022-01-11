module HW3.Pretty
  ( prettyValue
  ) where

import           Data.Foldable                 (toList)
import           Data.Map                      (Map, toList)
import           Data.Scientific               (fromRationalRepetendUnlimited)
import           Data.Sequence                 (Seq (..))
import           Data.Text                     (Text)
import           GHC.Real                      (Ratio (..))
import           HW3.Base
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction function) = prettyFunction function
prettyValue (HiValueNumber number)     = prettyNumber number
prettyValue (HiValueBool bool)         = prettyBool bool
prettyValue (HiValueString string)     = prettyString string
prettyValue HiValueNull                = prettyNull
prettyValue (HiValueList elements)     = HW3.Pretty.prettyList elements
prettyValue (HiValueDict dict)         = prettyDict dict

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
prettyFunction HiFunLength         = pretty "length"
prettyFunction HiFunToUpper        = pretty "to-upper"
prettyFunction HiFunToLower        = pretty "to-lower"
prettyFunction HiFunReverse        = pretty "reverse"
prettyFunction HiFunTrim           = pretty "trim"
prettyFunction HiFunList           = pretty "list"
prettyFunction HiFunRange          = pretty "range"
prettyFunction HiFunFold           = pretty "fold"
prettyFunction HiFunCount          = pretty "count"
prettyFunction HiFunKeys           = pretty "keys"
prettyFunction HiFunValues         = pretty "values"
prettyFunction HiFunInvert         = pretty "invert"

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

prettyString :: Text -> Doc AnsiStyle
prettyString = viaShow

prettyNull :: Doc AnsiStyle
prettyNull = pretty "null"

prettyList :: Seq HiValue -> Doc AnsiStyle
prettyList = list . Data.Foldable.toList . fmap prettyValue

prettyDict :: Map HiValue HiValue -> Doc AnsiStyle
prettyDict dict = case null dict of
  True -> pretty "{}"
  False -> group . encloseSep open close separator . fmap prettyKeyValue . Data.Map.toList $ dict
    where
      prettyKeyValue (x, y) = surround (pretty ": ") (prettyValue x) (prettyValue y)
      open                  = flatAlt (pretty "{ ") (pretty "{ ")
      close                 = flatAlt (pretty " }") (pretty " }")
      separator             = pretty ", "
