{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Wart.Kind.Syntax where

import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.UnionFind

bot :: (MonadSupply Int m, MonadUnionFind f m)
    => ReaderT (Binding f) m (f (Node f))

star :: (MonadSupply Int m, MonadUnionFind f m)
     => ReaderT (Binding f) m (f (Node f))

row :: (MonadSupply Int m, MonadUnionFind f m)
     => ReaderT (Binding f) m (f (Node f))

(-->) :: (MonadSupply Int m, MonadUnionFind f m)
      => ReaderT (Binding f) m (f (Node f))
      -> ReaderT (Binding f) m (f (Node f))
      -> ReaderT (Binding f) m (f (Node f))

data Binding (f :: * -> *)

data Node (f :: * -> *)
