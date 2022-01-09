{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module HW3.Classes
  ( Boolean(..)
  , Equalable(..)
  , Comparable(..)
  , Conditional(..)
  , Iterable(..)
  , Multiplable(..)
  , Indexable(..)
  , Viewable(..)
  , Stringable(..)
  , (~/~)
  ) where

import           Data.Semigroup (stimes)
import           Data.Text

infixr 4 #==#, #!=#, #<=#, #>=#, #<#, #>#
infixr 3 #&&#
infixr 2 #||#

class Boolean a where
  (#&&#), (#||#) :: a -> a -> a
  (#!#) :: a -> a
  fromBool :: Bool -> a

instance Boolean Bool where
  (#&&#)   = (&&)
  (#||#)   = (||)
  (#!#)    = not
  fromBool = id

class (Boolean b) => Equalable a b where
  (#==#), (#!=#) :: a -> a -> b
  x #!=# y = (#!#) $ x #==# y

instance (Eq a) => Equalable a Bool where
  (#==#) = (==)

class (Equalable a b, Boolean b) => Comparable a b where
  (#<=#), (#>=#), (#<#), (#>#) :: a -> a -> b
  x #<# y  = (x #!=# y) #&&# (x #<=# y)
  x #># y  = (#!#) $ x #<=# y
  x #>=# y = (#!#) $ x #<# y

instance (Eq a, Ord a) => Comparable a Bool where
  (#<=#) = (<=)

class (Boolean b) => Conditional a b where
  (#?:#) :: b -> a -> a -> a

class Iterable a where
  (~<~) :: a -> a
  (~+~) :: a -> a -> a

instance Iterable [a] where
  (~<~) = Prelude.reverse
  (~+~) = (<>)

instance Iterable Text where
  (~<~) = Data.Text.reverse
  (~+~) = (<>)

class (Num b) => Multiplable a b where
  (~*~) :: a -> b -> a

instance Multiplable [a] Integer where
  (~*~) = flip stimes

instance Multiplable Text Integer where
  (~*~) = flip stimes

class (Num b) => Indexable a b where
  (~#~) :: a -> b
  (~$~) :: a -> b -> a
  (~|~) :: a -> b -> b -> a

instance Indexable [a] Int where
  (~#~)       = Prelude.length
  (~$~) s i   = [(!!) s i]
  (~|~) s i j = Prelude.take (j - i) (Prelude.drop i s)

instance Indexable Text Int where
  (~#~)       = Data.Text.length
  (~$~) s i   = singleton $ index s i
  (~|~) s i j = Data.Text.take (j - i) (Data.Text.drop j s)

instance Indexable a Int => Indexable a Integer where
  (~#~) s     = toInteger ((~#~) s :: Int)
  (~$~) s i   = (~$~) s (fromInteger i :: Int) 
  (~|~) s i j = (~|~) s (fromInteger i :: Int)  (fromInteger j :: Int) 

class (Num b, Indexable a b) => Viewable a b c where
  (~@~) :: a -> b -> c

instance Viewable [a] Int a where
  (~@~) = (!!)

instance Viewable Text Int Char where
  (~@~) = index

instance Viewable a Int b => Viewable a Integer b where
  (~@~) s i = (~@~) s (fromInteger i :: Int)

class Stringable a where
  (~^~), (~%~), (~~~) :: a -> a
  fromText :: Text -> a
  fromString :: String -> a
  fromString = fromText . pack

instance Stringable Text where
  (~^~)    = toUpper
  (~%~)    = toLower
  (~~~)    = strip
  fromText = id

(~/~) :: (Stringable a, Iterable a) => a -> a -> a
(~/~) a b = (~+~) a $ (~+~) (fromString "/") b
