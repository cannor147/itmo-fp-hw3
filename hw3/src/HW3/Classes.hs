{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

instance Indexable [a] Integer where
  (~#~)     = toInteger . Prelude.length
  (~$~) s i = [(!!) s x]
    where
      x = fromInteger i
  (~|~) s i j = Prelude.take (y - x) (Prelude.drop x s)
    where
      x = fromInteger i
      y = fromInteger j

instance Indexable Text Integer where
  (~#~)       = toInteger . Data.Text.length
  (~$~) s i   = singleton $ index s x
    where
      x = fromInteger i
  (~|~) s i j = Data.Text.take (y - x) (Data.Text.drop x s)
    where
      x = fromInteger i
      y = fromInteger j

class (Num b, Indexable a b) => Viewable a b c where
  (~@~) :: a -> b -> c
  fromElement :: c -> a

instance Viewable [a] Integer a where
  (~@~) s i     = (!!) s $ fromInteger i
  fromElement x = [x]

instance Viewable Text Integer Char where
  (~@~) s i   = index s $ fromInteger i
  fromElement = singleton

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
