{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Kind.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Extras
import Control.Monad.Reader
import Control.Monad.UnionFind
import Language.Wart.Kind.Syntax as Kind
import Language.Wart.Node

class MonadUnionFind f m => Unify f m where
  merge :: f (Kind.Node f) -> f (Kind.Node f) -> m ()
  graft :: f (Kind.Node f) -> f (Kind.Node f) -> m ()
  throwKindError :: f (Kind.Node f) -> f (Kind.Node f) -> m b

#ifndef HLINT
  default merge :: (MonadTrans t, Unify f m)
                => f (Kind.Node f) -> f (Kind.Node f) -> t m ()
  merge v_x v_y = lift $ merge v_x v_y
#endif

#ifndef HLINT
  default graft :: (MonadTrans t, Unify f m)
                => f (Kind.Node f) -> f (Kind.Node f) -> t m ()
  graft v_x v_y = lift $ graft v_x v_y
#endif

#ifndef HLINT
  default throwKindError :: (MonadTrans t, Unify f m)
                         => f (Kind.Node f) -> f (Kind.Node f) -> t m b
  throwKindError v_x v_y = lift $ throwKindError v_x v_y
#endif

instance Unify f m => Unify f (ReaderT r m)

#ifndef HLINT
unify :: Unify f m => f (Kind.Node f) -> f (Kind.Node f) -> m ()
unify v_x v_y = whenM (v_x /== v_y) $
  (,) <$> v_x^!contents.value <*> v_y^!contents.value >>= \ case
    (Bot, Bot) -> merge v_x v_y
    (_, Bot) -> graft v_x v_y
    (Bot, _) -> graft v_y v_x
    (Star, Star) -> merge v_x v_y
    (Row, Row) -> merge v_x v_y
    (v_a :-> v_b, v_a' :-> v_b') -> do
      merge v_x v_y
      unify v_a v_a'
      unify v_b v_b'
    _ -> throwKindError v_x v_y
#endif
