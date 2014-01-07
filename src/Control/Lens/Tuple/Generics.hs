{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Lens.Tuple.Generics
       ( Ixed
       , Ixed'
       , ix
       , Nat (..)
       , N0, N1, N2, N3, N4, N5, N6, N7, N8
       ) where

import Control.Lens.Iso
import Control.Lens.Lens
import Data.Proxy
import GHC.Generics (Generic (..), (:*:) (..), K1 (..), M1 (..), U1 (..))
import GHC.Generics.Lens.Extras
import Type.Nat

type Ixed n s t a b = (Generic s, Generic t, GIxed n (Rep s) (Rep t) a b)
type Ixed' n s a = Ixed n s s a a

ix :: Ixed n s t a b => f n -> Lens s t a b
{-# INLINE ix #-}
ix n = rep.gix n

#ifndef HLINT
type family GSize (f :: * -> *) :: Nat
#endif
type instance GSize U1 = S Z
type instance GSize (K1 i c) = S Z
type instance GSize (M1 i c f) = GSize f
type instance GSize (a :*: b) = GSize a :+ GSize b

#ifndef HLINT
class GIxed (n :: Nat) s t a b | n s -> a, n t -> b, n s b -> t, n t a -> s where
  gix :: f n -> Lens (s x) (t x) a b
#endif

instance GIxed N0 U1 U1 () () where
  {-# INLINE gix #-}
  gix _ = iso (const ()) (const U1)

instance GIxed N0 (K1 i a) (K1 i b) a b where
  {-# INLINE gix #-}
  gix _ = iso unK1 K1

instance GIxed n s t a b => GIxed n (M1 i c s) (M1 i c t) a b where
  {-# INLINE gix #-}
  gix n = iso unM1 M1 . gix n

instance (p ~ (GSize s :> n), p ~ (GSize t :> n), GIxed' p n s s' t t' a b)
      => GIxed n (s :*: s') (t :*: t') a b where
  {-# INLINE gix #-}
  gix = gix' (Proxy :: Proxy p)

class (p ~ (GSize s :> n), p ~ (GSize t :> n))
   => GIxed' p n s s' t t' a b | p n s s' -> a
                               , p n t t' -> b
                               , p n s s' b -> t t'
                               , p n t t' a -> s s' where
  gix' :: f p -> g n -> Lens ((s :*: s') x) ((t :*: t') x) a b

instance ((GSize s :> n) ~ True, (GSize t :> n) ~ True, GIxed n s t a b)
      => GIxed' True n s s' t s' a b where
  {-# INLINE gix' #-}
  gix' _ n f (s :*: s') = fmap (:*: s') $ gix n f s

instance ((GSize s :> n) ~ False, n' ~ (GSize s :- n), GIxed n' s' t' a b)
      => GIxed' False n s s' s t' a b where
  {-# INLINE gix' #-}
  gix' _ _ f (s :*: s') = fmap (s :*:) $ gix (Proxy :: Proxy n') f s'
