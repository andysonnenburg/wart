{-# LANGUAGE Rank2Types #-}
module Control.Lens.Tuple.Extras
       ( duplicated
       , first
       , second
       ) where

import Control.Lens

duplicated :: Getter s (s, s)
{-# INLINE duplicated #-}
duplicated = to $ \ a -> (a, a)

first :: Functor f => LensLike f s t a b -> LensLike f (s, c) (t, c) (a, c) (b, c)
{-# INLINE first #-}
first = undefined

second :: Functor f => LensLike f s t a b -> LensLike f (c, s) (c, t) (c, a) (c, b)
{-# INLINE second #-}
second = undefined
