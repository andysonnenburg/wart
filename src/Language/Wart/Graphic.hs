{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Graphic
       ( Node
       , Binder
       , HasLabel (..)
       , HasTerm (..)
       , HasBindingFlag (..)
       , HasBinder (..)
       , AsScheme (..)
       , AsType (..)
       , AsKind (..)
       ) where

import Control.Applicative
import Control.Lens
import qualified Control.Lens.Tuple.Generics as Tuple
import qualified Control.Lens.Union.Generics as Union
import Data.Proxy
import Language.Wart.Kind.Syntax (Kind)
import Language.Wart.Scheme.Syntax (Scheme)
import Language.Wart.Type.Syntax (Type)
import Type.Nat

data family Node (c :: * -> *) :: (* -> *) -> *
data family Binder (c :: * -> *) :: (* -> *) -> *

class Functor f => HasLabel f s where
  label :: LensLike' f s Int

#ifndef HLINT
  default label :: Tuple.Ixed' N0 s Int => LensLike' f s Int
  label = Tuple.ix (Proxy :: Proxy N0)
#endif

class HasTerm s where
  term :: Node c ~ s => Lens' (s v) (c (v (s v)))

#ifndef HLINT
  default term :: (Node c ~ s, Tuple.Ixed' N1 (s v) (c (v (s v))))
               => Lens' (s v) (c (v (s v)))
  term = Tuple.ix (Proxy :: Proxy N1)
#endif

class (Profunctor p, Functor f) => HasBindingFlag p f s a | s -> a where
  bindingFlag :: Optic' p f s a

#ifndef HLINT
  default bindingFlag :: (p ~ (->), Tuple.Ixed' N2 s a) => LensLike' f s a
  bindingFlag = Tuple.ix (Proxy :: Proxy N2)
#endif

class (Profunctor p, Functor f) => HasBinder p f s a | s -> a where
  binder :: Optic' p f s a

#ifndef HLINT
  default binder :: (p ~ (->), Tuple.Ixed' N3 s a) => LensLike' f s a
  binder = Tuple.ix (Proxy :: Proxy N3)
#endif

class (Profunctor p, Functor f) => AsScheme p f (s :: (* -> *) -> *) where
  _Scheme :: Optic' p f (s v) (Node Scheme v)

#ifndef HLINT
  default _Scheme :: (Choice p, Applicative f, Union.Ixed' N0 (s v) (Node Scheme v))
                  => Optic' p f (s v) (Node Scheme v)
  _Scheme = Union.ix (Proxy :: Proxy N0)
#endif

class AsType s where
  _Type :: Prism' (s v) (v (Node Type v))

#ifndef HLINT
  default _Type :: Union.Ixed' N1 (s v) (v (Node Type v))
                => Prism' (s v) (v (Node Type v))
  _Type = Union.ix (Proxy :: Proxy N1)
#endif

class AsKind s where
  _Kind :: Prism' (s v) (v (Node Kind v))

#ifndef HLINT
  default _Kind :: Union.Ixed' N2 (s v) (v (Node Kind v))
                => Prism' (s v) (v (Node Kind v))
  _Kind = Union.ix (Proxy :: Proxy N2)
#endif
