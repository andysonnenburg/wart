{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Type.Syntax
       ( Type (..)
       , _Bot, _Number, _String, _Fn, _Record, _Variant, _Empty, _Extend, _App
       , Arity
       , Label
       , Binding (..), bindingFlag, binder
       , Binder (..), IsBinder (..)
       , Node (..), kind
       ) where

import Control.Applicative
import Control.Lens (Choice,
                     Field1 (..),
                     Field2 (..),
                     Field3 (..),
                     Field4 (..),
                     Lens,
                     Lens',
                     Optic,
                     Prism,
                     Prism')
import Control.Lens.Union
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Wart.BindingFlag
import qualified Language.Wart.Kind.Syntax as Kind
import Language.Wart.Node
import qualified Language.Wart.Scheme.Syntax as Scheme

data Type a
  = Bot
  | Number
  | String
  | Fn {-# UNPACK #-} !Arity
  | Record
  | Variant
  | Empty
  | Extend {-# UNPACK #-} !Label
  | App a a deriving Generic
instance VariantA (Type a) (Type a) () ()
instance VariantB (Type a) (Type a) () ()
instance VariantC (Type a) (Type a) () ()
instance VariantD (Type a) (Type a) Arity Arity
instance VariantE (Type a) (Type a) () ()
instance VariantF (Type a) (Type a) () ()
instance VariantG (Type a) (Type a) () ()
instance VariantH (Type a) (Type a) Label Label
instance VariantI (Type a) (Type a') (a, a) (a', a')

_Bot :: Prism' (Type a) ()
_Bot = _A

_Number :: Prism' (Type a) ()
_Number = _B

_String :: Prism' (Type a) ()
_String = _C

_Fn :: Prism' (Type a) Arity
_Fn = _D

_Record :: Prism' (Type a) ()
_Record = _E

_Variant :: Prism' (Type a) ()
_Variant = _F

_Empty :: Prism' (Type a) ()
_Empty = _G

_Extend :: Prism' (Type a) Label
_Extend = _H

_App :: Prism (Type a) (Type a') (a, a) (a', a')
_App = _I

type Arity = Int
type Label = Text

data Binding f = Binding !BindingFlag !(Binder f) deriving Generic
instance Field1 (Binding f) (Binding f) BindingFlag BindingFlag
instance Field2 (Binding f) (Binding f') (Binder f) (Binder f')

bindingFlag :: Lens' (Binding f) BindingFlag
bindingFlag = _1

binder :: Lens (Binding f) (Binding f') (Binder f) (Binder f')
binder = _2

data Binder f
  = Scheme !(Scheme.Node f)
  | Type (f (Node f)) deriving Generic
instance VariantA (Binder f) (Binder f) (Scheme.Node f) (Scheme.Node f)
instance VariantB (Binder f) (Binder f) (f (Node f)) (f (Node f))

instance (Choice p, Applicative f) =>
         Scheme.IsBinder p f (Binder a) (Binder a) (Scheme.Node a) (Scheme.Node a) where
  _Scheme = _A

class (Choice p, Applicative f) => IsBinder p f s t a b | s -> a
                                                        , t -> b
                                                        , s b -> t
                                                        , t a -> s where
  _Type :: Optic p f s t a b

instance (Choice p, Applicative f) =>
         IsBinder p f (Binder a) (Binder a) (a (Node a)) (a (Node a)) where
  _Type = _B

data Node f =
  Node
  {-# UNPACK #-} !Int
  (f (Binding f))
  (Type (f (Node f)))
  (f (Kind.Node f)) deriving Generic
instance Field1 (Node f) (Node f) Int Int
instance Field2 (Node f) (Node f) (f (Binding f)) (f (Binding f))
instance Field3 (Node f) (Node f) (Type (f (Node f))) (Type (f (Node f)))
instance Field4 (Node f) (Node f) (f (Kind.Node f)) (f (Kind.Node f))

instance IsNode (Node f) (f (Binding f)) (Type (f (Node f))) where
  binding = _2
  value = _3

kind :: Lens' (Node f) (f (Kind.Node f))
kind = _4
