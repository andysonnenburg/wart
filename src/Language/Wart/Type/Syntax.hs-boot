{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Type.Syntax where

import Control.Lens

class (Choice p, Functor f) => IsBinder p f s a | s -> a where
  _Type :: Optic' p f s a

data Node (f :: * -> *)
