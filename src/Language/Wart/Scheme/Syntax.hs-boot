{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Scheme.Syntax where

import Control.Lens

data Scheme a

data Binding (f :: * -> *)

_Root :: Prism' (Binding f) ()
_Binder :: Prism (Binding f) (Binding f') (Binder f) (Binder f')

data Binder (f :: * -> *)

class (Profunctor p, Functor f) => IsBinder p f s t a b | s -> a
                                                        , t -> b
                                                        , s b -> t
                                                        , t a -> s where
  _Scheme :: Optic p f s t a b

data Node (f :: * -> *)
