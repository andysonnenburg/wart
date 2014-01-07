{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Lens.Union
       ( VariantA (..)
       , VariantB (..)
       , VariantC (..)
       , VariantD (..)
       , VariantE (..)
       , VariantF (..)
       , VariantG (..)
       , VariantH (..)
       , VariantI (..)
       ) where

import Control.Lens.Prism
import Control.Lens.Union.Generics
import Data.Proxy (Proxy (Proxy))

class VariantA s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _A :: Prism s t a b
#ifndef HLINT
  default _A :: Ixed N0 s t a b => Prism s t a b
  {-# INLINE _A #-}
  _A = ix (Proxy :: Proxy N0)
#endif

class VariantB s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _B :: Prism s t a b
#ifndef HLINT
  default _B :: Ixed N1 s t a b => Prism s t a b
  {-# INLINE _B #-}
  _B = ix (Proxy :: Proxy N1)
#endif

class VariantC s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _C :: Prism s t a b
#ifndef HLINT
  default _C :: Ixed N2 s t a b => Prism s t a b
  {-# INLINE _C #-}
  _C = ix (Proxy :: Proxy N2)
#endif

class VariantD s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _D :: Prism s t a b
#ifndef HLINT
  default _D :: Ixed N3 s t a b => Prism s t a b
  {-# INLINE _D #-}
  _D = ix (Proxy :: Proxy N3)
#endif

class VariantE s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _E :: Prism s t a b
#ifndef HLINT
  default _E :: Ixed N4 s t a b => Prism s t a b
  {-# INLINE _E #-}
  _E = ix (Proxy :: Proxy N4)
#endif

class VariantF s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _F :: Prism s t a b
#ifndef HLINT
  default _F :: Ixed N5 s t a b => Prism s t a b
  {-# INLINE _F #-}
  _F = ix (Proxy :: Proxy N5)
#endif

class VariantG s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _G :: Prism s t a b
#ifndef HLINT
  default _G :: Ixed N6 s t a b => Prism s t a b
  {-# INLINE _G #-}
  _G = ix (Proxy :: Proxy N6)
#endif

class VariantH s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _H :: Prism s t a b
#ifndef HLINT
  default _H :: Ixed N7 s t a b => Prism s t a b
  {-# INLINE _H #-}
  _H = ix (Proxy :: Proxy N7)
#endif

class VariantI s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _I :: Prism s t a b
#ifndef HLINT
  default _I :: Ixed N8 s t a b => Prism s t a b
  {-# INLINE _I #-}
  _I = ix (Proxy :: Proxy N8)
#endif

instance VariantA Bool Bool () ()
instance VariantB Bool Bool () ()
instance VariantA (Maybe a) (Maybe a) () ()
instance VariantB (Maybe a) (Maybe a') a a'
instance VariantA (Either a b) (Either a' b) a a'
instance VariantB (Either a b) (Either a b') b b'
