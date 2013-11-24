{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Type.Syntax where

import Control.Applicative
import Control.Lens
import Data.Text (Text)
import Language.Wart.BindingFlag

data Type a

_Bot :: Prism' (Type a) ()
_Number :: Prism' (Type a) ()
_String :: Prism' (Type a) ()
_Fn :: Prism' (Type a) Arity
_Record :: Prism' (Type a) ()
_Variant :: Prism' (Type a) ()
_Empty :: Prism' (Type a) ()
_Extend :: Prism' (Type a) Label
_App :: Prism (Type a) (Type a') (a, a) (a', a')

type Arity = Int
type Label = Text

data Binding (f :: * -> *)

bindingFlag :: Lens' (Binding f) BindingFlag
binder :: Lens (Binding f) (Binding f') (Binder f) (Binder f')

data Binder (f :: * -> *)

class (Choice p, Applicative f) => IsBinder p f s t a b | s -> a
                                                        , t -> b
                                                        , s b -> t
                                                        , t a -> s where
  _Type :: Optic p f s t a b

data Node (f :: * -> *)
