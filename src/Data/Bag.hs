{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Data.Bag
       ( Bag
       , singleton
       , insert
       , union
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Foldable (Foldable (foldMap))
import Data.Semigroup
import Data.Semigroup.Foldable
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
  (<>) = union

instance Functor Bag where
#ifndef HLINT
  fmap f = \ case
    Singleton x -> Singleton (f x)
    Insert x xs -> Insert (f x) (fmap f xs)
    Union xs ys -> Union (fmap f xs) (fmap f ys)
#endif

instance Foldable Bag where
#ifndef HLINT
  foldMap f = \ case
    Singleton x -> f x
    Insert x xs -> mappend (f x) (foldMap f xs)
    Union xs ys -> mappend (foldMap f xs) (foldMap f ys)
#endif

instance Foldable1 Bag where
#ifndef HLINT
  foldMap1 f = \ case
    Singleton x -> f x
    Insert x xs -> f x <> foldMap1 f xs
    Union xs ys -> foldMap1 f xs <> foldMap1 f ys
#endif

instance Traversable Bag where
#ifndef HLINT
  traverse f = \ case
    Singleton x -> Singleton <$> f x
    Insert x xs -> Insert <$> f x <*> traverse f xs
    Union xs ys -> Union <$> traverse f xs <*> traverse f ys
#endif
