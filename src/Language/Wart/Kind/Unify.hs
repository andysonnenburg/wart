{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Kind.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Applicative
import Control.Monad.Extras
import Control.Monad.UnionFind
import Language.Wart.Kind.Syntax
import Prelude hiding (read)

class Unify f m where
  throwKindMismatch :: Kind f -> Kind f -> m a

#ifndef HLINT
unify :: (MonadUnionFind f m, Unify f m) => f (Kind f) -> f (Kind f) -> m ()
unify f_x f_y = whenM (f_x /== f_y) $ (,) <$> read f_x <*> read f_y >>= \ case
  (_, Default) -> union f_x f_y
  (Default, _) -> union f_y f_x
  (Star, Star) -> union f_x f_y
  (Row, Row) -> union f_x f_y
  (f_a :-> f_b, f_a' :-> f_b') -> do
    union f_x f_y
    unify f_a f_a'
    unify f_b f_b'
  (k_x, k_y) -> throwKindMismatch k_x k_y
#endif
