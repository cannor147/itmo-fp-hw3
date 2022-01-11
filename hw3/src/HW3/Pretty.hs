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
import Data.ByteString (ByteString)
import Data.ByteString.Internal (unpackBytes)
import Numeric (showHex)
import Data.Word (Word8)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction function) = prettyFunction function
prettyValue (HiValueNumber number)     = prettyNumber number
prettyValue (HiValueBool bool)         = prettyBool bool
prettyValue (HiValueString string)     = prettyString string
prettyValue HiValueNull                = prettyNull
prettyValue (HiValueList elements)     = HW3.Pretty.prettyList elements
prettyValue (HiValueDict dict)         = prettyDict dict
prettyValue (HiValueBytes bytes)       = prettyBytes bytes

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
prettyFunction HiFunPackBytes      = pretty "pack-bytes"
prettyFunction HiFunUnpackBytes    = pretty "unpack-bytes"
prettyFunction HiFunEncodeUtf8     = pretty "encode-utf8"
prettyFunction HiFunDecodeUtf8     = pretty "decode-utf8"
prettyFunction HiFunZip            = pretty "zip"
prettyFunction HiFunUnzip          = pretty "unzip"
prettyFunction HiFunSerialise      = pretty "serialise"
prettyFunction HiFunDeserialise    = pretty "deserialise"

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
prettyList = prettyWith "[]" "[ " " ]" ", " . Data.Foldable.toList . fmap prettyValue

prettyKeyValue :: (HiValue, HiValue) -> Doc AnsiStyle
prettyKeyValue (x, y) = surround (pretty ": ") (prettyValue x) (prettyValue y)

prettyDict :: Map HiValue HiValue -> Doc AnsiStyle
prettyDict = prettyWith "{}" "{ " " }" ", " . fmap prettyKeyValue . Data.Map.toList

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte = pretty . (\x -> if length x == 1 then "0" <> x else x) . flip showHex ""

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes = prettyWith "[# #]" "[# " " #]" " " . fmap prettyByte . unpackBytes

prettyWith :: String -> String -> String -> String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyWith e o c s elements = if null elements then pretty e else prettyBody elements
  where
    prettyBody = group . encloseSep open close (pretty s)
    open       = flatAlt (pretty o) (pretty o)
    close      = flatAlt (pretty c) (pretty c)
