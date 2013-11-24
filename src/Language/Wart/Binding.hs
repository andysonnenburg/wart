{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Binding
       ( IsBinding (..), bindingFlag, binder
       ) where

import Control.Lens
import Language.Wart.BindingFlag

class (Profunctor p, Functor f) => IsBinding p f s a | s -> a where
  tupled :: Optic' p f s (BindingFlag, a)

bindingFlag :: IsBinding (->) f s a => LensLike' f s BindingFlag
bindingFlag = tupled._1

binder :: IsBinding (->) f s a => LensLike' f s a
binder = tupled._2
