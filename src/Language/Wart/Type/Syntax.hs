{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Type.Syntax
       ( Type (..)
       , _Bot, _Const, _App
       , Const (..)
       , _Number, _String, _Fn, _Record, _Variant, _Empty, _Extend
       , Arity
       , Label
       ) where

import Control.Lens (Prism, Prism')
import qualified Control.Lens.Union.Generics as Union
import Data.Foldable (Foldable)
import Data.Proxy
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Language.Wart.Label
import Type.Nat

data Type a
  = Bot
  | Const Const
  | App a a deriving (Functor, Foldable, Traversable, Generic)

_Bot :: Prism' (Type a) ()
_Bot = Union.ix (Proxy :: Proxy N0)

_Const :: Prism' (Type a) Const
_Const = Union.ix (Proxy :: Proxy N1)

_App :: Prism (Type a) (Type a') (a, a) (a', a')
_App = Union.ix (Proxy :: Proxy N2)

data Const
  = Number
  | String
  | Fn {-# UNPACK #-} !Arity
  | Record
  | Variant
  | Empty
  | Extend {-# UNPACK #-} !Label deriving (Eq, Generic)

_Number :: Prism' Const ()
_Number = Union.ix (Proxy :: Proxy N0)

_String :: Prism' Const ()
_String = Union.ix (Proxy :: Proxy N1)

_Fn :: Prism' Const Arity
_Fn = Union.ix (Proxy :: Proxy N2)

_Record :: Prism' Const ()
_Record = Union.ix (Proxy :: Proxy N3)

_Variant :: Prism' Const ()
_Variant = Union.ix (Proxy :: Proxy N4)

_Empty :: Prism' Const ()
_Empty = Union.ix (Proxy :: Proxy N5)

_Extend :: Prism' Const Label
_Extend = Union.ix (Proxy :: Proxy N6)

type Arity = Int
