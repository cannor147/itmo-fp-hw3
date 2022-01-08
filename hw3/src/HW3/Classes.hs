{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HW3.Classes
  ( Boolean(..)
  , Equalable(..)
  , Comparable(..)
  , Conditional(..)
  ) where

infixr 4 <==>, <!=>, <<=>, <>=>, <<>, <>>
infixr 3 <&&>
infixr 2 <||>

class Boolean a where
  (<&&>), (<||>) :: a -> a -> a
  (<!>) :: a -> a
  fromBool :: Bool -> a

instance Boolean Bool where
  (<&&>)   = (&&)
  (<||>)   = (||)
  (<!>)    = not
  fromBool = id

class (Boolean b) => Equalable a b where
  (<==>), (<!=>) :: a -> a -> b
  x <!=> y = (<!>) $ x <==> y

instance (Eq a) => Equalable a Bool where
  (<==>) = (==)

class (Equalable a b, Boolean b) => Comparable a b where
  (<<=>), (<>=>), (<<>), (<>>) :: a -> a -> b
  x <<> y  = (x <!=> y) <&&> (x <<=> y)
  x <>> y  = (<!>) $ x <<=> y
  x <>=> y = (<!>) $ x <<> y

instance (Eq a, Ord a) => Comparable a Bool where
  (<<=>) = (<=)

class (Boolean b) => Conditional a b where
  (<?:>) :: b -> a -> a -> a
