{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Wart.Syntax
       ( Expr (..)
       , Record (..)
       ) where

import Data.Array (Array)
import GHC.Generics (Generic)
import Language.Wart.Label

data Expr f a
  = Name a
  | Fn {-# UNPACK #-} !(Array Int a) (f (Expr f a))
  | Call (f (Expr f a)) {-# UNPACK #-} !(Array Int (f (Expr f a)))
  | Let a (f (Expr f a)) (f (Expr f a))
  | Var a (f (Expr f a)) (f (Expr f a))
  | f (Expr f a) `Then` f (Expr f a)
  | Try (f (Expr f a)) [Catch f a] (Maybe (f (Expr f a)))
  | Throw (f (Expr f a))
  | Return (f (Expr f a))
  | Void
  | Read {-# UNPACK #-} !Label
  | Write {-# UNPACK #-} !Label
  | Record (Record f a) (Maybe (f (Expr f a)))
  | Variant {-# UNPACK #-} !Label (f (Expr f a)) deriving Generic
deriving instance (Show (f (Expr f a)), Show a) => Show (Expr f a)

data Record f a
  = Empty
  | Extend {-# UNPACK #-} !Label (f (Expr f a)) !(Record f a) deriving Generic
deriving instance Show (f (Expr f a)) => Show (Record f a)

data Catch f a = Catch {-# UNPACK #-} !Label a (f (Expr f a))
deriving instance (Show (f (Expr f a)), Show a) => Show (Catch f a)
