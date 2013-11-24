{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Kind.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Lens
import Control.Monad.Extras
import Control.Monad.Reader
import Control.Monad.UnionFind
import Language.Wart.Kind.Syntax as Kind
import Language.Wart.Node
import Prelude (($), (>>=))

class MonadUnionFind f m => Unify f m where
  merged :: Kind.Node f -> Kind.Node f -> m ()
  grafted :: Kind.Node f -> Kind.Node f -> m ()
  throwKindError :: Kind (f (Kind.Node f)) -> Kind (f (Kind.Node f)) -> m b

#ifndef HLINT
  default merged :: (MonadTrans t, Unify f m) => Kind.Node f -> Kind.Node f -> t m ()
  merged n_x n_y = lift $ merged n_x n_y
#endif

#ifndef HLINT
  default grafted :: (MonadTrans t, Unify f m) => Kind.Node f -> Kind.Node f -> t m ()
  grafted n_x n_y = lift $ grafted n_x n_y
#endif

#ifndef HLINT
  default throwKindError :: (MonadTrans t, Unify f m)
                         => Kind (f (Kind.Node f)) -> Kind (f (Kind.Node f)) -> t m b
  throwKindError k_x k_y = lift $ throwKindError k_x k_y
#endif

instance Unify f m => Unify f (ReaderT r m)

#ifndef HLINT
unify :: Unify f m => f (Kind.Node f) -> f (Kind.Node f) -> m ()
unify v_x v_y = whenM (v_x /== v_y) $ do
  n_x <- read v_x
  n_y <- read v_y
  case (n_x^.value, n_y^.value) of
    (Bot, Bot) -> do
      merged n_x n_y
      union v_x v_y
    (_, Bot) -> do
      merged n_x n_y
      grafted n_x n_y
      union v_x v_y
    (Bot, _) -> do
      merged n_y n_x
      grafted n_y n_x
      union v_y v_x
    (Star, Star) -> do
      merged n_x n_y
      union v_x v_y
    (Row, Row) -> do
      merged n_x n_y
      union v_x v_y
    (v_a :-> v_b, v_a' :-> v_b') -> do
      merged n_x n_y
      union v_x v_y
      unify v_a v_a'
      unify v_b v_b'
    (k_x, k_y) -> throwKindError k_x k_y
  union (n_x^.binding) (n_y^.binding)
#endif
