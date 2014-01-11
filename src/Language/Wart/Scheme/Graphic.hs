{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Wart.Scheme.Graphic
       ( module Language.Wart.Graphic
       , module Language.Wart.Scheme.Syntax
       , Node (..)
       , Binder (..)
       , type'
       ) where

import Control.Applicative
import Control.Lens
import qualified Control.Lens.Iso.Generics as Iso
import qualified Control.Lens.Tuple.Generics as Tuple
import Data.Proxy
import GHC.Generics (Generic)
import Language.Wart.BindingFlag
import Language.Wart.Graphic
import Language.Wart.Scheme.Syntax
import Language.Wart.Type.Syntax (Type)
import Type.Nat

data instance Node Scheme v =
  Node
  {-# UNPACK #-} !Int
  (Maybe (Binder Scheme v))
  (v (Node Type v)) deriving Generic
instance Functor f => HasLabel f (Node Scheme v)

instance (Applicative f, Contravariant f)
      => HasBindingFlag (->) f (Node Scheme v) BindingFlag where
  bindingFlag = Tuple.ix (Proxy :: Proxy N1)._Just.to (const Flexible)

instance Applicative f => HasBinder (->) f (Node Scheme v) (Binder Scheme v) where
  binder = Tuple.ix (Proxy :: Proxy N1)._Just

newtype instance Binder Scheme v = Scheme (Node Scheme v) deriving Generic
instance (Profunctor p, Functor f) => AsScheme p f (Binder Scheme) where
  _Scheme = from Iso.wrapped

instance Functor f => HasLabel f (Binder Scheme v) where
  label = _Scheme.label

type' :: Lens' (Node Scheme v) (v (Node Type v))
type' = Tuple.ix (Proxy :: Proxy N2)
