{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Binding
       ( IsBinding (..), bindingFlag, binder
       ) where

import Control.Lens
import Language.Wart.BindingFlag

class (Profunctor p, Functor f) => IsBinding p f s t a | s -> a
                                                       , t -> a
                                                       , a -> s t where
  tupled :: Optic p f s t (BindingFlag, a) (BindingFlag, a)

bindingFlag :: IsBinding (->) f s t a => LensLike f s t BindingFlag BindingFlag
bindingFlag = tupled._1

binder :: IsBinding (->) f s t a => LensLike f s t a a
binder = tupled._2
