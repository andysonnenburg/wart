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

instance (IsGTuple s, IsGTuple s', IsGTuple t, IsGTuple t',
          IsTuple (GList (s :*: s')), IsTuple (GList (t :*: t')),
          a ~ ToTuple (s :*: s'), b ~ ToTuple (t :*: t'))
      => GIxed N0 (s :*: s') (t :*: t') a b where
  {-# INLINE gix #-}
  gix _ = dimap (toTuple . toGTuple) (fmap $ fromGTuple . fromTuple)

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

#ifndef HLINT
data GTuple xs where
  U :: GTuple '[]
  (:*) :: x -> !(GTuple xs) -> GTuple (x ': xs)
#endif

infixr 5 :*

#ifndef HLINT
uncons :: (a -> GTuple as -> r) -> GTuple (a ': as) -> r
{-# INLINE uncons #-}
uncons f (a :* as) = f a as
#endif

#ifndef HLINT
unnil :: r -> GTuple '[] -> r
{-# INLINE unnil #-}
unnil r U = r
#endif

class IsTuple xs where
  type Tuple xs
  toTuple :: GTuple xs -> Tuple xs
  fromTuple :: Tuple xs -> GTuple xs

#ifndef HLINT
instance IsTuple '[] where
  type Tuple '[] = ()
  {-# INLINE toTuple #-}
  toTuple _ = ()
  {-# INLINE fromTuple #-}
  fromTuple _ = U
#endif

#ifndef HLINT
instance IsTuple '[a] where
  type Tuple '[a] = a
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    unnil a
  {-# INLINE fromTuple #-}
  fromTuple a = a :* U
#endif

#ifndef HLINT
instance IsTuple [a, b] where
  type Tuple [a, b] = (a, b)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    unnil (a, b)
  {-# INLINE fromTuple #-}
  fromTuple (a, b) = a :* b :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c] where
  type Tuple [a, b, c] = (a, b, c)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    unnil (a, b, c)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c) = a :* b :* c :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d] where
  type Tuple [a, b, c, d] = (a, b, c, d)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    unnil (a, b, c, d)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d) = a :* b :* c :* d :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e] where
  type Tuple [a, b, c, d, e] = (a, b, c, d, e)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    unnil (a, b, c, d, e)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e) = a :* b :* c :* d :* e :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e, f] where
  type Tuple [a, b, c, d, e, f] = (a, b, c, d, e, f)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    uncons $ \ f ->
    unnil (a, b, c, d, e, f)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f) = a :* b :* c :* d :* e :* f :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e, f, g] where
  type Tuple [a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    uncons $ \ f ->
    uncons $ \ g ->
    unnil (a, b, c, d, e, f, g)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f, g) = a :* b :* c :* d :* e :* f :* g :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e, f, g, h] where
  type Tuple [a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    uncons $ \ f ->
    uncons $ \ g ->
    uncons $ \ h ->
    unnil (a, b, c, d, e, f, g, h)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f, g, h) = a :* b :* c :* d :* e :* f :* g :* h :* U
#endif

#ifndef HLINT
instance IsTuple [a, b, c, d, e, f, g, h, i] where
  type Tuple [a, b, c, d, e, f, g, h, i] = (a, b, c, d, e, f, g, h, i)
  {-# INLINE toTuple #-}
  toTuple =
    uncons $ \ a ->
    uncons $ \ b ->
    uncons $ \ c ->
    uncons $ \ d ->
    uncons $ \ e ->
    uncons $ \ f ->
    uncons $ \ g ->
    uncons $ \ h ->
    uncons $ \ i ->
    unnil (a, b, c, d, e, f, g, h, i)
  {-# INLINE fromTuple #-}
  fromTuple (a, b, c, d, e, f, g, h, i) = a :* b :* c :* d :* e :* f :* g :* h :* i :* U
#endif

type ToTuple s = Tuple (GList s)

class IsGTuple s where
  type GCons s xs
  gcons :: s x -> GTuple xs -> GTuple (GCons s xs)
  guncons :: (s x -> GTuple xs -> r) -> GTuple (GCons s xs) -> r

#ifndef HLINT
type GList s = GCons s '[]
#endif

toGTuple :: IsGTuple s => s x -> GTuple (GList s)
{-# INLINE toGTuple #-}
toGTuple = flip gcons U

fromGTuple :: IsGTuple s => GTuple (GList s) -> s x
{-# INLINE fromGTuple #-}
fromGTuple = guncons unnil

instance IsGTuple U1 where
  type GCons U1 xs = xs
  {-# INLINE gcons #-}
  gcons = flip const
  {-# INLINE guncons #-}
  guncons = ($ U1)

instance IsGTuple (K1 i c) where
#ifndef HLINT
  type GCons (K1 i c) xs = c ': xs
#endif
  {-# INLINE gcons #-}
  gcons = (:*) . unK1
  {-# INLINE guncons #-}
  guncons f = uncons $ f . K1

instance IsGTuple f => IsGTuple (M1 i c f) where
  type GCons (M1 i c f) xs = GCons f xs
  {-# INLINE gcons #-}
  gcons = gcons . unM1
  {-# INLINE guncons #-}
  guncons f = guncons $ f . M1

instance (IsGTuple a, IsGTuple b) => IsGTuple (a :*: b) where
  type GCons (a :*: b) xs = GCons a (GCons b xs)
  {-# INLINE gcons #-}
  gcons (a :*: b) = gcons a . gcons b
  {-# INLINE guncons #-}
  guncons f = guncons $ \ a -> guncons $ \ b -> f $ a :*: b

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
