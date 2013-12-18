module Data.Bag
       ( Bag
       , singleton
       , insert
       , union
       ) where

data Bag a = One a | Many (Bag a) (Bag a)

singleton :: a -> Bag a
{-# INLINE singleton #-}
singleton = One

insert :: a -> Bag a -> Bag a
{-# INLINE insert #-}
insert = Many . One

union :: Bag a -> Bag a -> Bag a
{-# INLINE union #-}
union = Many
