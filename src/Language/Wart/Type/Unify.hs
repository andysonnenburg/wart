{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Type.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Applicative (Applicative (..), (<$>))
import Control.Lens ((^.), (^!), (%~), _1, _2, re, to)
import Control.Lens.Switch
import Control.Lens.Tuple.Extras
import Control.Monad.Extras
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.Trans.Maybe
import Control.Monad.UnionFind
import Language.Wart.Binding
import Language.Wart.Kind (Kind (Star, Row, (:->)), throwKindError)
import qualified Language.Wart.Kind as Kind
import Language.Wart.Node
import Language.Wart.Type.Syntax (Const (..),
                                  Label,
                                  Type (App, Bot, Const),
                                  _App, _Bot, _Const, _Extend,
                                  cloneBinder, kind)
import qualified Language.Wart.Type.Syntax as Type
import Prelude (Bool (..), Int, Maybe (..),
                ($), (.), (==), flip, id, otherwise, undefined)

class (MonadSupply Int m, Kind.Unify f m) => Unify f m where
  merge :: f (Type.Node f) -> f (Type.Node f) -> m ()
  graft :: f (Type.Node f) -> f (Type.Node f) -> m ()
  throwTypeError :: Type (f (Type.Node f)) -> Type (f (Type.Node f)) -> m b
  throwRowError :: Label -> Type (f (Type.Node f)) -> m b

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
                         => Type (f (Type.Node f)) -> Type (f (Type.Node f)) -> t m b
  throwTypeError t_x t_y = lift $ throwTypeError t_x t_y
#endif

#ifndef HLINT
  default throwRowError :: (MonadTrans t, Unify f m)
                        => Label -> Type (f (Type.Node f)) -> t m b
  throwRowError l t = lift $ throwRowError l t
#endif

instance Unify f m => Unify f (ReaderT r m)

#ifndef HLINT
unify :: Unify f m => f (Type.Node f) -> f (Type.Node f) -> m ()
unify v_x v_y = whenM (v_x /== v_y) $ do
  n_x <- read v_x
  n_y <- read v_y
  Kind.unify (n_x^.kind) (n_y^.kind)
  n_x^!kind.contents.value >>= \ case
    Row -> case (n_x^.value, n_y^.value) of
      (Bot, Bot) -> merge v_x v_y
      (_, Bot) -> graft v_x v_y
      (Bot, _) -> graft v_y v_x
      (Const Empty, Const Empty) -> merge v_x v_y
      (t_x, t_y) -> switch t_x
        $ caseM (_App.
                 first (duplicated.second (contents.value._App._1.
                                           contents.value._Const._Extend))).:
        (\ ((v_l_t, l), v_r) -> do
          (v_y', v_l_t', v_r') <- withoutTailOf v_x $ unifyRow l v_y
          merge v_x v_y
          merge v_x v_y'
          unify v_l_t v_l_t'
          unify v_r v_r')
        $ default'
        (\ _-> throwTypeError t_x t_y)
    _ -> case (n_x^.value, n_y^.value) of
      (Bot, Bot) -> merge v_x v_y
      (_, Bot) -> graft v_x v_y
      (Bot, _) -> graft v_y v_x
      (Const c, Const c') | c == c'-> merge v_x v_y
      (App t1 t2, App t1' t2') -> do
        merge v_x v_y
        unify t1 t1'
        unify t2 t2'
      (t_x, t_y) -> throwTypeError t_x t_y
#endif

#ifndef HLINT
unifyRow :: (Unify f m, MonadReader (f (Type.Node f)) m, MonadSupply Int m)
         => Label
         -> f (Type.Node f)
         -> m (f (Type.Node f), f (Type.Node f), f (Type.Node f))
unifyRow l = fix (\ rec v_r0 -> read v_r0 >>= \ n_r0 -> switch (n_r0^.value)
  $ caseM _Bot.:
  (\ () -> withBinding (n_r0^.binding) $ do
    v_r1 <- bot
    v_l_t1 <- app (const $ Extend l) bot
    join $ graft <$> app (pure v_l_t1) (pure v_r1) <*> pure v_r0
    return (v_r0, v_l_t1, v_r1))
  $ caseM (_App.
           first (duplicated.second (contents.value._App._1.
                                     contents.value._Const._Extend))).:
  (\ ((v_l_t1, l'), v_r1) ->
    if l' == l
    then return (v_r0, v_l_t1, v_r1)
    else do
      n_r1 <- read v_r1
      (v_r1', v_l_t2, v_r2) <- unifyRow l v_r1
      merge v_r1 v_r1'
      v_r1'' <- withBinding (n_r0^.binding) $ app (pure v_l_t1) (pure v_r2)
      v_r0' <- withBinding (n_r1^.binding) $ app (pure v_l_t2) (pure v_r1'')
      return (v_r0', v_l_t2, v_r1''))
  $ default'
  (\ t0 -> throwRowError l t0))
#endif

withoutTailOf :: f (Type.Node f) -> ReaderT (f (Type.Node f)) m a -> m a
withoutTailOf = flip runReaderT

isTailOf :: MonadUnionFind f m => f (Type.Node f) -> f (Type.Node f) -> m Bool
isTailOf = undefined

withBinding :: f (Type.Binding f) -> ReaderT (f (Type.Binding f)) m a -> m a
withBinding = flip runReaderT

bot :: (MonadReader (f (Type.Binding f)) m, MonadSupply Int m, MonadUnionFind f m)
    => m (f (Type.Node f))
bot = do
  b_t <- read =<< ask
  v_b_t <- new b_t
  new =<< Type.newNode v_b_t Bot =<< kindBot

const :: (MonadReader (f (Type.Binding f)) m,
           MonadSupply Int m,
           MonadUnionFind f m)
       => Const -> m (f (Type.Node f))
const c = do
  b_t <- read =<< ask
  v_b_t <- new b_t
  new =<< Type.newNode v_b_t (Const c) =<< lam star (lam row row)

star :: m (f (Kind.Node f))
star = undefined

lam :: m (f (Kind.Node f)) -> m (f (Kind.Node f)) -> m (f (Kind.Node f))
lam = undefined

row :: m (f (Kind.Node f))
row = undefined

kindBot :: m (f (Kind.Node f))
kindBot = undefined

app :: (MonadReader (f (Type.Binding f)) m, MonadSupply Int m, Kind.Unify f m)
    => m (f (Type.Node f))
    -> m (f (Type.Node f))
    -> m (f (Type.Node f))
app m_a m_b = do
  b_t <- read =<< ask
  v_b_t <- new b_t
  v_a <- m_a
  v_b <- m_b
  v_k <- kindBot
  join $ Kind.unify <$> v_a^!contents.kind <*> lam (v_b^!contents.kind) (pure v_k)
  new =<< Type.newNode v_b_t (App v_a v_b) v_k
