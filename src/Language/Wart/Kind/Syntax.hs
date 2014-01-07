{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Kind.Syntax
       ( Kind (..)
       , _Bot, _Star, _Row
       ) where

import Control.Lens
import qualified Control.Lens.Union.Generics as Union
import Data.Foldable (Foldable)
import Data.Proxy
import GHC.Generics (Generic)
import Type.Nat

data Kind a
  = Bot
  | Star
  | Row
  | a :-> a deriving (Functor, Foldable, Traversable, Generic)

_Bot :: Prism' (Kind a) ()
_Bot = Union.ix (Proxy :: Proxy N0)

_Star :: Prism' (Kind a) ()
_Star = Union.ix (Proxy :: Proxy N1)

_Row :: Prism' (Kind a) ()
_Row = Union.ix (Proxy :: Proxy N2)
