{-# LANGUAGE DeriveGeneric #-}
module Language.Wart.Scheme.Syntax
       ( Scheme (..)
       , Binding (..)
       , Binder (..)
       ) where

import GHC.Generics (Generic)
import Language.Wart.Type

data Scheme f = G (f (Maybe (Scheme f))) (f (BoundType f)) deriving Generic

data BoundType f = Type (f (Binding f)) (f (Type f))

data Binding f = Binder BindingFlag (Binder f)

data Binder f = Scheme (f (Scheme f)) | BoundType (f (BoundType f))

data BindingFlag = Rigid | Flexible deriving Generic
