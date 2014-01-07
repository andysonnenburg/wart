{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Lens.Iso.Generics (Wrapped, wrapped) where

import Control.Lens (Iso, from, iso)
import GHC.Generics (Generic, Rep, K1 (..), M1 (..), U1 (..))
import GHC.Generics.Lens.Extras

type Wrapped s t a b = (Generic a, Generic b, GWrapped s t (Rep a) (Rep b))

wrapped :: Wrapped s t a b => Iso s t a b
{-# INLINE wrapped #-}
wrapped = gwrapped.from rep

class GWrapped s t a b | a -> s, b -> t, a t -> s, b s -> t where
  gwrapped :: Iso s t (a x) (b x)

instance GWrapped () () U1 U1 where
  {-# INLINE gwrapped #-}
  gwrapped = iso (const U1) (const ())

instance GWrapped s t (K1 i s) (K1 i t) where
  {-# INLINE gwrapped #-}
  gwrapped = iso K1 unK1

instance GWrapped s t a b => GWrapped s t (M1 i c a) (M1 i c b) where
  {-# INLINE gwrapped #-}
  gwrapped = gwrapped.iso M1 unM1
