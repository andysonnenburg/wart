{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
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
module Control.Lens.Union.Generics
       ( Ixed
       , Ixed'
       , ix
       , Nat (..)
       , N0, N1, N2, N3, N4, N5, N6, N7, N8
       ) where

import Control.Applicative
import Control.Lens.Iso
import Control.Lens.Prism
import Control.Lens.Tuple.Internal
import Data.Proxy
import GHC.Generics (Generic (..), (:+:) (..), (:*:) (..), K1 (..), M1 (..), U1 (..))
import GHC.Generics.Lens.Extras
import Type.Nat

type Ixed n s t a b = (Generic s, Generic t, GIxed n (Rep s) (Rep t) a b)
type Ixed' n s a = Ixed n s s a a

ix :: Ixed n s t a b => f n -> Prism s t a b
{-# INLINE ix #-}
ix n = rep.gix n

#ifndef HLINT
class GIxed (n :: Nat) s t a b | s -> a, t -> b, s b -> t, t a -> s where
  gix :: f n -> Prism (s x) (t x) a b
#endif

instance GIxed N0 U1 U1 () () where
  {-# INLINE gix #-}
  gix _ = prism (const U1) (const $ Right ())

instance GIxed N0 (K1 i a) (K1 i b) a b where
  {-# INLINE gix #-}
  gix _ = iso unK1 K1

instance GIxed n s t a b => GIxed n (M1 i c s) (M1 i c t) a b where
  {-# INLINE gix #-}
  gix n = iso unM1 M1 . gix n

instance GIxed' (GSize s :> n) n s s' t t' a b
      => GIxed n (s :+: s') (t :+: t') a b where
  {-# INLINE gix #-}
  gix = gix' (Proxy :: Proxy (GSize s :> n))

instance (GIsHList s, GIsHList t, GIsHList s', GIsHList t',
          IsHList a, List a ~ GCons s (GCons s' '[]),
          IsHList b, List b ~ GCons t (GCons t' '[]))
      => GIxed N0 (s :*: s') (t :*: t') a b where
  {-# INLINE gix #-}
  gix _ = iso (fromHList . gtoHList) (gfromHList . toHList)

#ifndef HLINT
class GIxed' (p :: Bool) (n :: Nat) s s' t t' a b | s s' -> a
                                                  , t t' -> b
                                                  , s s' b -> t t'
                                                  , t t' a -> s s' where
  gix' :: f p -> g n -> Prism ((s :+: s') x) ((t :+: t') x) a b
#endif

instance (GIxed n s t a b, s' ~ t') => GIxed' True n s s' t t' a b where
  {-# INLINE gix' #-}
  gix' _ n = dimap (gsum Left Right) (either (fmap L1) (pure . R1)) . left' . gix n

instance (GIxed (GSize s :- n) s' t' a b, s ~ t)
      => GIxed' False n s s' t t' a b where
  {-# INLINE gix' #-}
  gix' _ _ = dimap (gsum Left Right) (either (pure . L1) (fmap R1)) . right' .
    gix (Proxy :: Proxy (GSize s :- n))

gsum :: (a x -> r) -> (b x -> r) -> (a :+: b) x -> r
{-# INLINE gsum #-}
gsum f _ (L1 a) = f a
gsum _ f (R1 a) = f a

#ifndef HLINT
type family GSize (f :: * -> *) :: Nat
#endif
type instance GSize U1 = S Z
type instance GSize (K1 i c) = S Z
type instance GSize (M1 i c f) = GSize f
type instance GSize (a :+: b) = GSize a :+ GSize b
type instance GSize (a :*: b) = S Z
