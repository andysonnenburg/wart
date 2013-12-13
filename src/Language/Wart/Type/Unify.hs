{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Wart.Type.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Applicative (Applicative (..), (<$>))
import Control.Lens (Effective, LensLike', (^.), (^!), _1, _2)
import Control.Lens.Switch
import Control.Lens.Tuple.Extras
import Control.Monad.Extras
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.UnionFind
import Language.Wart.Kind (Kind, (~>), _Row, row, star)
import qualified Language.Wart.Kind as Kind
import Language.Wart.Node
import Language.Wart.Type.Syntax (Label,
                                  Type (App, Bot, Const),
                                  _App, _Bot, _Const, _Extend,
                                  app, bot,
                                  extend,
                                  kind)
import qualified Language.Wart.Type.Syntax as Type
import Prelude (Bool (..), Int, Maybe (..),
                ($), (.), (==), const, flip, id, otherwise, undefined)

class (MonadSupply Int m, Kind.Unify f m) => Unify f m where
  merge :: f (Type.Node f) -> f (Type.Node f) -> m ()
  graft :: f (Type.Node f) -> f (Type.Node f) -> m ()
  throwTypeError :: f (Type.Node f) -> f (Type.Node f) -> m a
  throwRowError :: Label -> f (Type.Node f) -> m a

#ifndef HLINT
  default merge :: (MonadTrans t, Unify f m)
                => f (Type.Node f) -> f (Type.Node f) -> t m ()
  merge v_x v_y = lift $ merge v_x v_y
#endif

#ifndef HLINT
  default graft :: (MonadTrans t, Unify f m)
                => f (Type.Node f) -> f (Type.Node f) -> t m ()
  graft v_x v_y = lift $ graft v_x v_y
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

instance Unify f m => Unify f (ReaderT r m)

unify :: Unify f m => f (Type.Node f) -> f (Type.Node f) -> m ()
unify v_x v_y = whenM (v_x /== v_y) $ do
  n_x <- read v_x
  n_y <- read v_y
  Kind.unify (n_x^.kind) (n_y^.kind)
  switch n_x
    $ caseM (kinded _Row.value.extension).: (\ ((v_l_t, l), v_r) -> do
      (v_l_t', v_r') <- withoutTailOf v_x $ getRow l v_y
      merge v_x v_y
      unify v_l_t v_l_t'
      unify v_r v_r')
    $ default' $ case (n_x^.value, n_y^.value) of
      (Bot, Bot) -> merge v_x v_y
      (_, Bot) -> graft v_x v_y
      (Bot, _) -> graft v_y v_x
      (Const c, Const c') | c == c'-> merge v_x v_y
      (App t1 t2, App t1' t2') -> do
        merge v_x v_y
        unify t1 t1'
        unify t2 t2'
      _ -> throwTypeError v_x v_y

getRow :: (Unify f m, MonadReader (f (Type.Node f)) m)
       => Label
       -> f (Type.Node f)
       -> m (f (Type.Node f), f (Type.Node f))
getRow l v_r0 = read v_r0 >>= \ n_r0 -> switch (n_r0^.value)
  $ case' _Bot.: (\ () -> do
    whenM (isTailOf v_r0 =<< ask) $ throwTypeError v_r0 =<< ask
    withBindingOf n_r0 $ do
      v_r1 <- bot row
      v_l_t1 <- app (extend l) (bot star) (row ~> row)
      join $ graft <$> app (pure v_l_t1) (pure v_r1) row <*> pure v_r0
      return (v_l_t1, v_r1))
  $ caseM extension.: (\ ((v_l_t1, l'), v_r1) ->
    if l' == l
    then return (v_l_t1, v_r1)
    else do
      n_r1 <- read v_r1
      (v_l_t2, v_r2) <- getRow l v_r1
      v_r3 <- withBindingOf n_r0 $ app (pure v_l_t1) (pure v_r2) row
      v_r0' <- withBindingOf n_r1 $ app (pure v_l_t2) (pure v_r3) row
      merge v_r0 v_r0'
      return (v_l_t2, v_r3))
  $ default' $ throwRowError l v_r0

withoutTailOf :: f (Type.Node f) -> ReaderT (f (Type.Node f)) m a -> m a
withoutTailOf = flip runReaderT

isTailOf :: MonadUnionFind f m => f (Type.Node f) -> f (Type.Node f) -> m Bool
v_x `isTailOf` v_y =
  switch v_y
  $ caseM (contents.value._App._2).:
  (\ v_ys -> v_x === v_ys)
  $ default' $ return False

withBindingOf :: MonadUnionFind f m
              => Type.Node f
              -> ReaderT (Type.Binding f) m a
              -> m a
withBindingOf n m = runReaderT m =<< n^!binding.contents

kinded :: (Effective m r f, MonadUnionFind var m)
       => LensLike' (First (Type.Node var) f) (Kind (var (Kind.Node var))) a
       -> LensLike' f (Type.Node var) (Type.Node var)
kinded l = duplicated.first (kind.contents.value.l)._2

extension :: (Applicative f, Effective m r f, MonadUnionFind var m)
          => LensLike' f
             (Type (var (Type.Node var)))
             ((var (Type.Node var), Label), var (Type.Node var))
extension = _App.first (duplicated.second (contents.value._App._1.
                                           contents.value._Const._Extend))
