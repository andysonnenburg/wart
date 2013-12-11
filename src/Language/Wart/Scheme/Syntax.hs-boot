{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Scheme.Syntax where

import Control.Lens

class (Profunctor p, Functor f) => IsBinder p f s a | s -> a where
  _Scheme :: Optic' p f s a

data Binder (f :: * -> *)

data Node (f :: * -> *)

instance Functor f => IsBinder (->) f (Binder a) (Node a)
