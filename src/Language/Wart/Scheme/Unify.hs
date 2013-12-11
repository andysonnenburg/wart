{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Scheme.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Monad.Supply
import Control.Monad.Trans.Class
import Control.Monad.UnionFind
import qualified Language.Wart.Kind as Kind
import qualified Language.Wart.Scheme.Syntax as Scheme
import Language.Wart.Type (Label)
import qualified Language.Wart.Type as Type

class (MonadSupply Int m, MonadUnionFind f m) => Unify f m where
  throwKindError :: f (Kind.Node f) -> f (Kind.Node f) -> m a
  throwTypeError :: f (Type.Node f) -> f (Type.Node f) -> m a
  throwRowError :: Label -> f (Type.Node f) -> m a
  throwSchemeError :: Scheme.Node f -> Scheme.Node f -> m a

#ifndef HLINT
  default throwKindError :: (MonadTrans t, Unify f m)
                         => f (Kind.Node f) -> f (Kind.Node f) -> t m b
  throwKindError v_x v_y = lift $ throwKindError v_x v_y
#endif

#ifndef HLINT
  default throwTypeError :: (MonadTrans t, Unify f m)
                         => f (Type.Node f) -> f (Type.Node f) -> t m a
  throwTypeError v_x v_y = lift $ throwTypeError v_x v_y
#endif

#ifndef HLINT
  default throwRowError :: (MonadTrans t, Unify f m)
                        => Label -> f (Type.Node f) -> t m a
  throwRowError l v_r = lift $ throwRowError l v_r
#endif

#ifndef HLINT
  default throwSchemeError :: (MonadTrans t, Unify f m)
                           => Scheme.Node f -> Scheme.Node f -> t m a
  throwSchemeError x y = lift $ throwSchemeError x y
#endif

unify :: Unify f m
      => Scheme.Node f
      -> f (Type.Node f)
      -> f (Type.Node f)
      -> m ()
unify _ _ _ = return ()
