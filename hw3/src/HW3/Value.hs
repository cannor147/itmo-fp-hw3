{-# LANGUAGE MultiParamTypeClasses #-}

module HW3.Value 
  ( Value(..)
  , UnaryOperator
  , BinaryOperator
  , TernaryOperator
  , (.!.)
  , (.#.)
  , (.^.)
  , (.+.)
  , (.$.)
  , bytesToList
  , bytesToValue
  , cd'
  , er
  , er2
  , fold
  , fromDict
  , fromUtf8
  , listToBytes
  , mkdir'
  , range
  , read'
  , toUtf8
  , unzip'
  , valueToBytes
  , write'
  , zip'
  ) where

import           Codec.Compression.Zlib
import           Codec.Serialise
import           Data.ByteString          (ByteString)
import           Data.ByteString.Internal (unpackBytes)
import qualified Data.ByteString.Lazy     as LazyByteString
import           Data.Foldable            (foldl', toList)
import           Data.Map                 (Map, fromListWith, keys, lookup,
                                           toList)
import           Data.Sequence            (Seq (..), fromList)
import           Data.Text                (Text, unpack)
import           Data.Text.Encoding       (decodeUtf8', encodeUtf8)
import           Data.Word                (Word8)
import           GHC.Real                 (Ratio (..))
import           HW3.Base
import           HW3.Classes

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

arithmetic2 :: (Rational -> Rational -> Rational) -> BinaryOperator
arithmetic2 op (Val (HiValueNumber a)) (Val (HiValueNumber b)) = Val $ HiValueNumber $ op a b
arithmetic2 _  a                       b                       = er2 a b

fromChar :: Char -> Value
fromChar = fromText . fromElement

fromByte :: Word8 -> Value
fromByte = fromInteger . toInteger

fromByteInt :: Integer -> Value
fromByteInt = fromBytes . fromElement . (fromInteger :: Integer -> Word8)

fromSeq :: Seq HiValue -> Value
fromSeq = Val . HiValueList

fromDict :: Map HiValue HiValue -> Value
fromDict = Val . HiValueDict

fromBytes :: ByteString -> Value
fromBytes = Val . HiValueBytes

fromLazyBytes :: LazyByteString.ByteString -> Value
fromLazyBytes = fromBytes . LazyByteString.toStrict

fromAction :: HiAction -> Value
fromAction = Val . HiValueAction

hiNull :: Value
hiNull = Val HiValueNull

instance Num Value where
  abs                                                     = arithmetic abs
  signum                                                  = arithmetic signum
  (+) a@(Val (HiValueString _)) b@(Val (HiValueString _)) = (~+~) a b
  (+) a@(Val (HiValueList _))   b@(Val (HiValueList _))   = (~+~) a b
  (+) a@(Val (HiValueBytes _))  b@(Val (HiValueBytes _))  = (~+~) a b
  (+) a                         b                         = arithmetic2 (+) a b
  (-)                                                     = arithmetic2 (-)
  (*) a@(Val (HiValueString _)) b@(Val (HiValueNumber _)) = (~*~) a b
  (*) a@(Val (HiValueList _))   b@(Val (HiValueNumber _)) = (~*~) a b
  (*) a@(Val (HiValueBytes _))  b@(Val (HiValueNumber _)) = (~*~) a b
  (*) a                         b                         = arithmetic2 (*) a b
  fromInteger                                             = Val . HiValueNumber . fromInteger

instance Fractional Value where
  (/) a@(Val (HiValueString _)) b@(Val (HiValueString _)) = (~/~) a b
  (/) _                         (Val (HiValueNumber 0))   = Er HiErrorDivideByZero
  (/) a                         b                         = arithmetic2 (/) a b
  fromRational                                            = Val . HiValueNumber

instance Boolean Value where
  (#&&#) (Val (HiValueBool False)) _ = Val $ HiValueBool False
  (#&&#) (Val HiValueNull)         _ = Val HiValueNull
  (#&&#) _                         b = b
  (#||#) (Val (HiValueBool False)) b = b
  (#||#) (Val HiValueNull)         b = b
  (#||#) a                         _ = a
  (#!#)                              = boolean not
  fromBool                           = Val . HiValueBool

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
  (~<~) (Val (HiValueString a)) = fromText  $ (~<~) a
  (~<~) (Val (HiValueList a))   = fromSeq   $ (~<~) a
  (~<~) (Val (HiValueBytes a))  = fromBytes $ (~<~) a
  (~<~) a                       = er a
  (~+~) (Val (HiValueString a)) (Val (HiValueString b)) = fromText  $ (~+~) a b
  (~+~) (Val (HiValueList a))   (Val (HiValueList b))   = fromSeq   $ (~+~) a b
  (~+~) (Val (HiValueBytes a))  (Val (HiValueBytes b))  = fromBytes $ (~+~) a b
  (~+~) a                       b                       = er2 a b

negEr :: (Ord a, Num a) => a -> Value -> Value
negEr a b = if a <= 0 then Er HiErrorInvalidArgument else b

instance Sliceable Value Value where
  (~#~) (Val (HiValueString a)) = fromInteger $ (~#~) a
  (~#~) (Val (HiValueList a))   = fromInteger $ (~#~) a
  (~#~) (Val (HiValueBytes a))  = fromInteger $ (~#~) a
  (~#~) a                       = er a
  (~*~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = negEr b $ fromText  $ (~*~) a b
  (~*~) (Val (HiValueList a))   (Val (HiValueNumber (b :% 1))) = negEr b $ fromSeq   $ (~*~) a b
  (~*~) (Val (HiValueBytes a))  (Val (HiValueNumber (b :% 1))) = negEr b $ fromBytes $ (~*~) a b
  (~*~) a                       b                              = er2 a b
  (~^~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = fromText  $ (~^~) a b
  (~^~) (Val (HiValueList a))   (Val (HiValueNumber (b :% 1))) = fromSeq   $ (~^~) a b
  (~^~) (Val (HiValueBytes a))  (Val (HiValueNumber (b :% 1))) = fromBytes $ (~^~) a b
  (~^~) a                       b                              = er2 a b
  (~$~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = fromText  $ (~$~) a b
  (~$~) (Val (HiValueList a))   (Val (HiValueNumber (b :% 1))) = fromSeq   $ (~$~) a b
  (~$~) (Val (HiValueBytes a))  (Val (HiValueNumber (b :% 1))) = fromBytes $ (~$~) a b
  (~$~) a                       b                              = er2 a b

instance Contentable Value Value where
  (~&~)       (Val (HiValueList a)) (Val b) = fromSeq $ (~&~) a b
  (~&~)       a                     b       = er2 a b
  fromElement (Val a)        = fromSeq $ fromElement a
  fromElement hiError@(Er _) = hiError

indexSafely :: (Ord b, Iterable a b t) => a -> b -> (t -> Value) -> Value
indexSafely a b c = if b < 0 || b >= (~#~) a then hiNull else c $ (~@~) a b

instance Iterable Value Value Value where
  (~@~) (Val (HiValueDict a))   (Val b)                        = maybe hiNull Val foundValue
    where
      foundValue = Data.Map.lookup b a
  (~@~) (Val (HiValueString a)) (Val (HiValueNumber (b :% 1))) = indexSafely a b fromChar 
  (~@~) (Val (HiValueList a))   (Val (HiValueNumber (b :% 1))) = indexSafely a b Val
  (~@~) (Val (HiValueBytes a))  (Val (HiValueNumber (b :% 1))) = indexSafely a b fromByte
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
(.#.) (Val (HiValueString a))  = fromDict $ toCountMap $ HiValueString . fromElement <$> unpack a
(.#.) (Val (HiValueList a))    = fromDict $ toCountMap a
(.#.) a@(Val (HiValueBytes _)) = (.#.) $ bytesToList a
(.#.) a                        = er a

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

fold :: HiMonad m => (HiFun -> [HiExpr] -> m Value) -> Value -> Value -> m Value
fold _       (Val (HiValueFunction _)) (Val (HiValueList Empty))    = pure $ Val HiValueNull
fold applier (Val (HiValueFunction f)) (Val (HiValueList elements)) = do
  let values = fmap (pure . Val) elements
  flip foldl1 values $ \a b -> a >>= \x -> b >>= \y -> case (x, y) of
    (Val i, Val j) -> applier f [HiExprValue i, HiExprValue j]
    (i, j)         -> pure $ er2 i j
fold _       a                         b                            = pure $ er2 a b

bytesToList :: Value -> Value
bytesToList (Val (HiValueBytes a)) = fromSeq $ fromList $ hiValueInteger <$> unpackBytes a
  where
    hiValueInteger = HiValueNumber . toRational . toInteger
bytesToList a                      = er a

listToBytes :: Value -> Value
listToBytes (Val (HiValueList a)) = foldl' (~+~) (Val $ HiValueBytes mempty) $ fmap (toByte . Val) a
  where
    toByte b@(Val (HiValueNumber (n :% 1))) = if n >= 0 && n < 256 then fromByteInt n else er b
    toByte b                                = er b
listToBytes a                      = er a

fromUtf8 :: Value -> Value
fromUtf8 (Val (HiValueString a)) = fromBytes $ encodeUtf8 a
fromUtf8 a                       = er a

toUtf8 :: Value -> Value
toUtf8 (Val (HiValueBytes a)) = either (const hiNull) fromText $ decodeUtf8' a
toUtf8 a                      = er a

valueToBytes :: Value -> Value
valueToBytes (Val a) = fromBytes $ LazyByteString.toStrict $ serialise a
valueToBytes hiError = hiError

bytesToValue :: Value -> Value
bytesToValue (Val (HiValueBytes a)) = Val $ deserialise $ LazyByteString.fromStrict a
bytesToValue a                      = er a

zip' :: Value -> Value
zip' (Val (HiValueBytes a)) = fromLazyBytes $ compressWith params $ LazyByteString.fromStrict a
  where
    params = defaultCompressParams { compressLevel = bestCompression }
zip' a                      = er a

unzip' :: Value -> Value
unzip' (Val (HiValueBytes a)) = fromLazyBytes $ decompressWith params $ LazyByteString.fromStrict a
  where
    params = defaultDecompressParams
unzip' a                      = er a

read' :: Value -> Value
read' (Val (HiValueString a)) = fromAction $ HiActionRead $ unpack a
read' a                       = er a

fromWrite :: Text -> ByteString -> Value
fromWrite a b = fromAction $ HiActionWrite (unpack a) b

write' :: Value -> Value -> Value
write' (Val (HiValueString a)) (Val (HiValueString b)) = fromWrite a (encodeUtf8 b)
write' (Val (HiValueString a)) (Val (HiValueBytes b))  = fromWrite a b
write' a                         b                     = er2 a b

mkdir' :: Value -> Value
mkdir' (Val (HiValueString a)) = fromAction $ HiActionMkDir $ unpack a
mkdir' a                       = er a

cd' :: Value -> Value
cd' (Val (HiValueString a)) = fromAction $ HiActionChDir $ unpack a
cd' a                       = er a
