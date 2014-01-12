{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Lens.Tuple.Internal
       ( HList (..)
       , IsHList (..)
       , toHListDefault
       , fromHListDefault
       , GIsHList (..)
       , GList
       , gtoHList
       , gfromHList
       ) where

import GHC.Generics

infixr 5 :*

#ifndef HLINT
data family HList (xs :: [*]) :: *
data instance HList '[] = U
data instance HList (x ': xs) = x :* HList xs
#endif

class IsHList a where
  type List a
  toHList :: a -> HList (List a)
  fromHList :: HList (List a) -> a

#ifndef HLINT
  type List a = GList (Rep a)
  default toHList :: (Generic a, GIsHList (Rep a)) => a -> HList (GList (Rep a))
  {-# INLINE toHList #-}
  toHList = toHListDefault
  default fromHList :: (Generic a, GIsHList (Rep a)) => HList (GList (Rep a)) -> a
  {-# INLINE fromHList #-}
  fromHList = fromHListDefault
#endif

instance IsHList ()
instance IsHList (a, b)
instance IsHList (a, b, c)
instance IsHList (a, b, c, d)
instance IsHList (a, b, c, d, e)
instance IsHList (a, b, c, d, e, f)
instance IsHList (a, b, c, d, e, f, g)

#ifndef HLINT
toHListDefault :: (Generic a, GIsHList (Rep a)) => a -> HList (GList (Rep a))
{-# INLINE toHListDefault #-}
toHListDefault = gtoHList . from
#endif

#ifndef HLINT
fromHListDefault :: (Generic a, GIsHList (Rep a)) => HList (GList (Rep a)) -> a
{-# INLINE fromHListDefault #-}
fromHListDefault = to . gfromHList
#endif

class GIsHList a where
  type GCons a xs
  gcons :: a x -> HList xs -> HList (GCons a xs)
  guncons :: (a x -> HList xs -> r) -> HList (GCons a xs) -> r

type GList a = GCons a '[]

gtoHList :: GIsHList a => a x -> HList (GList a)
{-# INLINE gtoHList #-}
gtoHList = flip gcons U

gfromHList :: GIsHList a => HList (GList a) -> a x
{-# INLINE gfromHList #-}
gfromHList = guncons $ flip $ \ U -> id

instance GIsHList U1 where
  type GCons U1 xs = xs
  {-# INLINE gcons #-}
  gcons _ = id
  {-# INLINE guncons #-}
  guncons = ($ U1)

instance GIsHList (K1 i c) where
#ifndef HLINT
  type GCons (K1 i c) xs = c ': xs
#endif
  {-# INLINE gcons #-}
  gcons = (:*) . unK1
  {-# INLINE guncons #-}
  guncons f (x :* xs) = f (K1 x) xs

instance GIsHList f => GIsHList (M1 i c f) where
  type GCons (M1 i c f) xs = GCons f xs
  {-# INLINE gcons #-}
  gcons = gcons . unM1
  {-# INLINE guncons #-}
  guncons f = guncons $ f . M1

instance (GIsHList a, GIsHList b) => GIsHList (a :*: b) where
  type GCons (a :*: b) xs = GCons a (GCons b xs)
  {-# INLINE gcons #-}
  gcons (a :*: b) = gcons a . gcons b
  {-# INLINE guncons #-}
  guncons f = guncons $ \ a -> guncons $ \ b -> f (a :*: b)
