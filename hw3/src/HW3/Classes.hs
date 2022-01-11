{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module HW3.Classes
  ( Boolean(..)
  , Comparable(..)
  , Conditional(..)
  , Contentable(..)
  , Equalable(..)
  , Iterable(..)
  , Sequenceable(..)
  , Sliceable(..)
  , Stringable(..)
  , (~/~)
  ) where

import           Data.ByteString
import           Data.Semigroup  (stimes)
import           Data.Sequence
import           Data.Text
import           Data.Word       (Word8)

infixr 4 #==#, #!=#, #<=#, #>=#, #<#, #>#
infixr 3 #&&#
infixr 2 #||#

-- | Analogue for Bool.
class Boolean a where
  (#&&#), (#||#) :: a -> a -> a
  (#!#) :: a -> a
  fromBool :: Bool -> a

instance Boolean Bool where
  (#&&#)   = (&&)
  (#||#)   = (||)
  (#!#)    = not
  fromBool = id

-- | Analogue for Eq.
class (Boolean b) => Equalable a b where
  (#==#), (#!=#) :: a -> a -> b
  x #!=# y = (#!#) $ x #==# y

instance (Eq a) => Equalable a Bool where
  (#==#) = (==)

-- | Analogue for Ord.
class (Equalable a b, Boolean b) => Comparable a b where
  (#<=#), (#>=#), (#<#), (#>#) :: a -> a -> b
  x #<# y  = (x #!=# y) #&&# (x #<=# y)
  x #># y  = (#!#) $ x #<=# y
  x #>=# y = (#!#) $ x #<# y

instance (Eq a, Ord a) => Comparable a Bool where
  (#<=#) = (<=)

-- | Class that supports if statement.
class (Boolean b) => Conditional a b where
  (#?:#) :: b -> a -> a -> a

-- | Analogue for Monoid with reverse.
class Sequenceable a where
  (~~~) :: a
  (~+~) :: a -> a -> a
  (~<~), (~>~) :: a -> a
  (~>~) = id

instance Sequenceable [a] where
  (~~~) = mempty
  (~<~) = Prelude.reverse
  (~+~) = (<>)

instance Sequenceable (Seq a) where
  (~~~) = mempty
  (~<~) = Data.Sequence.reverse
  (~+~) = (<>)

instance Sequenceable Text where
  (~~~) = mempty
  (~<~) = Data.Text.reverse
  (~+~) = (<>)

instance Sequenceable ByteString where
  (~~~) = mempty
  (~<~) = Data.ByteString.reverse
  (~+~) = (<>)

-- | Class that supports slices.
class Num b => Sliceable a b where
  (~#~) :: a -> b
  (~*~), (~^~), (~$~) :: a -> b -> a
  (~!~) :: a -> b -> b -> a
  (~!~) l i j = (l ~$~ i) ~^~ (j - i)

instance Sliceable [a] Int where
  (~#~)       = Prelude.length
  (~*~)       = flip stimes
  (~^~)       = flip Prelude.take
  (~$~)       = flip Prelude.drop

instance Sliceable (Seq a) Int where
  (~#~)       = Prelude.length
  (~*~)       = flip stimes
  (~^~)       = flip Data.Sequence.take
  (~$~)       = flip Data.Sequence.drop

instance Sliceable Text Int where
  (~#~)       = Data.Text.length
  (~*~)       = flip stimes
  (~^~)       = flip Data.Text.take
  (~$~)       = flip Data.Text.drop

instance Sliceable ByteString Int where
  (~#~)       = Data.ByteString.length
  (~*~)       = flip stimes
  (~^~)       = flip Data.ByteString.take
  (~$~)       = flip Data.ByteString.drop

instance Sliceable a Int => Sliceable a Integer where
  (~#~)       = fromInt . (~#~)
  (~*~) l i   = (~*~) l $ toInt i
  (~^~) l i   = (~^~) l $ toInt i
  (~$~) l i   = (~$~) l $ toInt i

-- | Class that has content.
class Contentable a c where
  (~&~) :: a -> c -> a
  fromElement :: c -> a

instance Contentable [a] a where
  (~&~) l e     = l ++ [e]
  fromElement e = [e]

instance Contentable (Seq a) a where
  (~&~)       = (|>)
  fromElement = Data.Sequence.singleton

instance Contentable Text Char where
  (~&~)       = Data.Text.snoc
  fromElement = Data.Text.singleton

instance Contentable ByteString Word8 where
  (~&~)       = Data.ByteString.snoc
  fromElement = Data.ByteString.singleton

-- | Analogue for List.
class (Sliceable a b, Contentable a c) => Iterable a b c where
  (~@~) :: a -> b -> c

instance Iterable [a] Int a where
  (~@~) = (!!)

instance Iterable (Seq a) Int a where
  (~@~) = Data.Sequence.index

instance Iterable Text Int Char where
  (~@~) = Data.Text.index

instance Iterable ByteString Int Word8 where
  (~@~) = Data.ByteString.index

instance (Contentable a b, Iterable a Int b) => Iterable a Integer b where
  (~@~) l i = (~@~) l $ toInt i

-- | Analogue for String.
class Stringable a where
  (~|~), (~.~), (~-~) :: a -> a
  fromText :: Text -> a
  fromString :: String -> a
  fromString = fromText . Data.Text.pack

instance Stringable Text where
  (~|~)    = toUpper
  (~.~)    = toLower
  (~-~)    = strip
  fromText = id

-- | Concatenates two strings and separates them with slash.
(~/~) :: (Stringable a, Sequenceable a) => a -> a -> a
(~/~) a b = (~+~) a $ (~+~) (fromString "/") b

fromInt :: Int -> Integer
fromInt = fromIntegral

toInt :: Integer -> Int
toInt = fromInteger
