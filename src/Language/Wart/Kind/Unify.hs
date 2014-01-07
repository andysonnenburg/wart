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
import Language.Wart.Kind.Graphic as Kind

class MonadUnionFind v m => Unify v m where
  merge :: v (Node Kind v) -> v (Node Kind v) -> m ()
  graft :: v (Node Kind v) -> v (Node Kind v) -> m ()
  throwKindError :: Kind (v (Node Kind v)) -> Kind (v (Node Kind v)) -> m b

#ifndef HLINT
  default merge :: (MonadTrans t, Unify v m)
                => v (Node Kind v) -> v (Node Kind v) -> t m ()
  merge v_x v_y = lift $ merge v_x v_y
#endif

#ifndef HLINT
  default graft :: (MonadTrans t, Unify v m)
                => v (Node Kind v) -> v (Node Kind v) -> t m ()
  graft v_x v_y = lift $ graft v_x v_y
#endif

#ifndef HLINT
  default throwKindError :: (MonadTrans t, Unify v m)
                         => Kind (v (Node Kind v)) -> Kind (v (Node Kind v)) -> t m b
  throwKindError t_x t_y = lift $ throwKindError t_x t_y
#endif

instance Unify v m => Unify v (ReaderT r m)

#ifndef HLINT
unify :: Unify v m => v (Node Kind v) -> v (Node Kind v) -> m ()
unify v_x v_y = whenM (v_x /== v_y) $
  (,) <$> v_x^!contents.term <*> v_y^!contents.term >>= \ case
    (Bot, Bot) -> merge v_x v_y
    (_, Bot) -> graft v_x v_y
    (Bot, _) -> graft v_y v_x
    (Star, Star) -> merge v_x v_y
    (Row, Row) -> merge v_x v_y
    (v_a :-> v_b, v_a' :-> v_b') -> do
      merge v_x v_y
      unify v_a v_a'
      unify v_b v_b'
    (t_x, t_y) -> throwKindError t_x t_y
#endif
