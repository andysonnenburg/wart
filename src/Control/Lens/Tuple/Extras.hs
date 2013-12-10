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

duplicated :: Getter a (a, a)
duplicated = to $ \ a -> (a, a)

first :: Functor f
      => LensLike (First c f) s t a b
      -> LensLike f (s, c) (t, c) (a, c) (b, c)
{-# INLINE first #-}
first l f (a, c) = over _2 (fromMaybe c) <$> getFirst (l (\ b ->
  First $ over _2 Just <$> f (b, c)) a)

newtype First c f a = First { getFirst :: f (a, Maybe c) }

instance Functor f => Functor (First c f) where
  {-# INLINE fmap #-}
  fmap f = First . fmap (Arrow.first f) . getFirst

instance Applicative f => Applicative (First c f) where
  {-# INLINE pure #-}
  pure a = First $ pure (a, Nothing)
  {-# INLINE (<*>) #-}
  f <*> a =
    First $
    (\ (f', _) (a', _) -> (f' a', Nothing)) <$>
    getFirst f <*>
    getFirst a

instance Contravariant f => Contravariant (First c f) where
  {-# INLINE contramap #-}
  contramap f = First . contramap (Arrow.first f) . getFirst

instance Effective m r f => Effective m r (First c f) where
  effective = First . effective
  ineffective = ineffective . getFirst

second :: Functor f
       => LensLike (Second c f) s t a b
       -> LensLike f (c, s) (c, t) (c, a) (c, b)
{-# INLINE second #-}
second l f (c, a) = over _1 (fromMaybe c) <$> getSecond (l (\ b ->
  Second $ over _1 Just <$> f (c, b)) a)

newtype Second c f a = Second { getSecond :: f (Maybe c, a) }

instance Functor f => Functor (Second c f) where
  {-# INLINE fmap #-}
  fmap f = Second . fmap (Arrow.second f) . getSecond

instance Applicative f => Applicative (Second c f) where
  {-# INLINE pure #-}
  pure a = Second $ pure (Nothing, a)
  {-# INLINE (<*>) #-}
  f <*> a =
    Second $
    (\ (_, f') (_, a') -> (Nothing, f' a')) <$>
    getSecond f <*> getSecond a

instance (c ~ d, Contravariant f) => Contravariant (Second c f) where
  {-# INLINE contramap #-}
  contramap f = Second . contramap (Arrow.second f) . getSecond

instance Effective m r f => Effective m r (Second c f) where
  effective = Second . effective
  ineffective = ineffective . getSecond
