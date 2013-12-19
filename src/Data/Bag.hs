{-# LANGUAGE LambdaCase #-}
module Data.Bag
       ( Bag
       , singleton
       , insert
       , union
       ) where

import Control.Applicative
import Data.Foldable (Foldable (foldMap))
import Data.Semigroup
import Data.Traversable (Traversable (traverse))

data Bag a
  = Singleton a
  | Insert a !(Bag a)
  | Union !(Bag a) !(Bag a)

singleton :: a -> Bag a
{-# INLINE singleton #-}
singleton = Singleton

insert :: a -> Bag a -> Bag a
{-# INLINE insert #-}
insert = Insert

union :: Bag a -> Bag a -> Bag a
{-# INLINE union #-}
union = Union

instance Semigroup (Bag a) where
  {-# INLINE (<>) #-}
  (<>) = Union

instance Functor Bag where
  fmap f = \ case
    Singleton x -> Singleton (f x)
    Insert x xs -> Insert (f x) (fmap f xs)
    Union xs ys -> Union (fmap f xs) (fmap f ys)

instance Foldable Bag where
  foldMap f = \ case
    Singleton x -> f x
    Insert x xs -> mappend (f x) (foldMap f xs)
    Union xs ys -> mappend (foldMap f xs) (foldMap f ys)

instance Traversable Bag where
  traverse f = \ case
    Singleton x -> Singleton <$> f x
    Insert x xs -> Insert <$> f x <*> traverse f xs
    Union xs ys -> Union <$> traverse f xs <*> traverse f ys
