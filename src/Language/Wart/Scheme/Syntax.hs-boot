{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Scheme.Syntax where

import Control.Lens

class (Profunctor p, Functor f) => IsBinder p f s a | s -> a where
  _Scheme :: Optic' p f s a

data Node (f :: * -> *)
