module Data.Stream (Stream (..)) where

infixr 5 :|

data Stream a = a :| Stream a
