{-# LANGUAGE KindSignatures #-}
module Language.Wart.Kind.Syntax where

import Control.Lens
import Language.Wart.BindingFlag

data Kind a

_Bot :: Prism' (Kind a) ()
_Star :: Prism' (Kind a) ()
_Row :: Prism' (Kind a) ()

data Binding (f :: * -> *)

bindingFlag :: Lens' (Binding f) BindingFlag
binder :: Lens (Binding f) (Binding f') (Binder f) (Binder f')

data Binder (f :: * -> *)

_Kind :: Prism (Binder f) (Binder f) (f (Node f)) (f (Node f))

data Node (f :: * -> *)
