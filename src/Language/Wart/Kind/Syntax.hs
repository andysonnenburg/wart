{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Kind.Syntax
       ( Kind (..), _Bot, _Star, _Row
       , Binding (..), bindingFlag, binder
       , Binder (..), _Kind
       , Node (..)
       ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Union
import GHC.Generics (Generic)
import Language.Wart.BindingFlag
import Language.Wart.Node
import qualified Language.Wart.Scheme.Syntax as Scheme
import {-# SOURCE #-} qualified Language.Wart.Type.Syntax as Type

data Kind a
  = Bot
  | Star
  | Row
  | a :-> a deriving Generic
instance VariantA (Kind a) (Kind a) () ()
instance VariantB (Kind a) (Kind a) () ()
instance VariantC (Kind a) (Kind a) () ()
instance VariantD (Kind a) (Kind a') (a, a) (a', a')

_Bot :: Prism' (Kind a) ()
_Bot = _A

_Star :: Prism' (Kind a) ()
_Star = _B

_Row :: Prism' (Kind a) ()
_Row = _C

data Binding f = Binding !BindingFlag !(Binder f) deriving Generic
instance Field1 (Binding f) (Binding f) BindingFlag BindingFlag
instance Field2 (Binding f) (Binding f') (Binder f) (Binder f')

bindingFlag :: Lens' (Binding f) BindingFlag
bindingFlag = _1

binder :: Lens (Binding f) (Binding f') (Binder f) (Binder f')
binder = _2

data Binder f
  = Scheme (Scheme.Node f)
  | Type (f (Type.Node f))
  | Kind (f (Node f)) deriving Generic
instance VariantA (Binder f) (Binder f) (Scheme.Node f) (Scheme.Node f)
instance VariantB (Binder f) (Binder f) (f (Type.Node f)) (f (Type.Node f))
instance VariantC (Binder f) (Binder f) (f (Node f)) (f (Node f))

instance (Choice p, Applicative f) =>
         Scheme.IsBinder p f (Binder a) (Binder a) (Scheme.Node a) (Scheme.Node a) where
  _Scheme = _A

instance (Choice p, Applicative f) =>
         Type.IsBinder p f (Binder a) (Binder a) (a (Type.Node a)) (a (Type.Node a)) where
  _Type = _B

_Kind :: Prism (Binder f) (Binder f) (f (Node f)) (f (Node f))
_Kind = _C

data Node f =
  Node
  {-# UNPACK #-} !Int
  (f (Binding f))
  (Kind (f (Node f))) deriving Generic
instance Field1 (Node f) (Node f) Int Int
instance Field2 (Node f) (Node f) (f (Binding f)) (f (Binding f))
instance Field3 (Node f) (Node f) (Kind (f (Node f))) (Kind (f (Node f)))

instance IsNode (Node f) (f (Binding f)) (Kind (f (Node f))) where
  binding = _2
  value = _3
