{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Lens.Tuple.Extras
       ( duplicated
       , First (..)
       , first
       , Second (..)
       , second
       ) where

import Control.Applicative
import qualified Control.Arrow as Arrow
import Control.Lens
import Control.Lens.Internal.Action
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mempty)
import qualified Data.Monoid as Monoid

duplicated :: Getter a (a, a)
duplicated = to $ \ a -> (a, a)

first :: Functor f
      => LensLike (First c f) s t a b
      -> LensLike f (s, c) (t, c) (a, c) (b, c)
{-# INLINE first #-}
first l f (a, c) = getFirst (l (\ b ->
  First $ f (b, c)&mapped._2 %~ review (_Wrapped._Just)) a)
  &mapped._2 %~ fromMaybe c . view _Wrapped

newtype First c f a = First { getFirst :: f (a, Monoid.First c) }

instance Functor f => Functor (First c f) where
  {-# INLINE fmap #-}
  fmap f = First . fmap (Arrow.first f) . getFirst

instance Applicative f => Applicative (First c f) where
  {-# INLINE pure #-}
  pure a = First $ pure (a, mempty)
  {-# INLINE (<*>) #-}
  f <*> a =
    First $
    (\ (f', x) (a', y) -> (f' a', mappend x y)) <$>
    getFirst f <*>
    getFirst a

instance Contravariant f => Contravariant (First c f) where
  {-# INLINE contramap #-}
  contramap f = First . contramap (Arrow.first f) . getFirst

instance Effective m r f => Effective m r (First c f) where
  {-# INLINE effective #-}
  effective = First . effective
  {-# INLINE ineffective #-}
  ineffective = ineffective . getFirst

second :: Functor f
       => LensLike (Second c f) s t a b
       -> LensLike f (c, s) (c, t) (c, a) (c, b)
{-# INLINE second #-}
second l f (c, a) = getSecond (l (\ b ->
  Second $ f (c, b)&mapped._1 %~ review (_Wrapped._Just)) a)
  &mapped._1 %~ fromMaybe c . view _Wrapped

newtype Second c f a = Second { getSecond :: f (Monoid.First c, a) }

instance Functor f => Functor (Second c f) where
  {-# INLINE fmap #-}
  fmap f = Second . fmap (Arrow.second f) . getSecond

instance Applicative f => Applicative (Second c f) where
  {-# INLINE pure #-}
  pure a = Second $ pure (mempty, a)
  {-# INLINE (<*>) #-}
  f <*> a =
    Second $
    (\ (x, f') (y, a') -> (mappend x y, f' a')) <$>
    getSecond f <*>
    getSecond a

instance Contravariant f => Contravariant (Second c f) where
  {-# INLINE contramap #-}
  contramap f = Second . contramap (Arrow.second f) . getSecond

instance Effective m r f => Effective m r (Second c f) where
  {-# INLINE effective #-}
  effective = Second . effective
  {-# INLINE ineffective #-}
  ineffective = ineffective . getSecond
