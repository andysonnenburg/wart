{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Scheme.Syntax
       ( Scheme (..)
       , Binding (..), _Root, _Binder
       , Binder (..), IsBinder (..)
       , Node (..), type'
       ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Union
import GHC.Generics (Generic)
import Language.Wart.Binding
import Language.Wart.Node
import {-# SOURCE #-} qualified Language.Wart.Type.Syntax as Type

data Scheme a = G deriving Generic

data Binding f = Root | Binder (Binder f) deriving Generic
instance VariantA (Binding f) (Binding f) () ()
instance VariantB (Binding f) (Binding f') (Binder f) (Binder f')

_Root :: Prism' (Binding f) ()
_Root = _A

_Binder :: Prism (Binding f) (Binding f') (Binder f) (Binder f')
_Binder = _B

newtype Binder f = Scheme (Node f) deriving Generic
instance Field1 (Binder f) (Binder f') (Node f) (Node f')

class (Profunctor p, Functor f) => IsBinder p f s a | s -> a where
  _Scheme :: Optic' p f s a

instance Functor f => IsBinder (->) f (Binder a) (Node a) where
  _Scheme = _1

data Node f =
  Node
  {-# UNPACK #-} !Int
  (f (Binding f))
  (Scheme (Node f))
  (f (Type.Node f)) deriving Generic
instance Field1 (Node f) (Node f) Int Int
instance Field2 (Node f) (Node f) (f (Binding f)) (f (Binding f))
instance Field3 (Node f) (Node f) (Scheme (Node f)) (Scheme (Node f))
instance Field4 (Node f) (Node f) (f (Type.Node f)) (f (Type.Node f))

instance IsNode (Node f) (f (Binding f)) (Scheme (Node f)) where
  binding = _2
  value = _3

type' :: Lens' (Node f) (f (Type.Node f))
type' = _4
