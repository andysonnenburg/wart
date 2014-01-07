{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Type.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Applicative (Applicative (..), (<$>))
import Control.Lens (Effective, LensLike', (^.), (^!), _1, _2, perform)
import Control.Lens.Switch
import Control.Lens.Tuple.Extras
import Control.Monad.Extras
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.UnionFind
import Language.Wart.BindingFlag
import Language.Wart.Graphic
import Language.Wart.Kind ((~>), _Row, row, star)
import qualified Language.Wart.Kind as Kind
import Language.Wart.Type.Graphic (Label,
                                   Type (App, Bot, Const),
                                   _App, _Bot, _Const, _Extend,
                                   app, bot,
                                   extend,
                                   kind)

class (MonadSupply Int m, Kind.Unify v m) => Unify v m where
  merge :: v (Node Type v) -> v (Node Type v) -> m ()
  graft :: v (Node Type v) -> v (Node Type v) -> m ()
  throwTypeError :: Type (v (Node Type v)) -> Type (v (Node Type v)) -> m a
  throwRowError :: Label -> Type (v (Node Type v)) -> m a

#ifndef HLINT
  default merge :: (MonadTrans t, Unify v m)
                => v (Node Type v) -> v (Node Type v) -> t m ()
  merge v_x v_y = lift $ merge v_x v_y
#endif

#ifndef HLINT
  default graft :: (MonadTrans t, Unify v m)
                => v (Node Type v) -> v (Node Type v) -> t m ()
  graft v_x v_y = lift $ graft v_x v_y
#endif

#ifndef HLINT
  default throwTypeError :: (MonadTrans t, Unify v m)
                         => Type (v (Node Type v)) -> Type (v (Node Type v)) -> t m a
  throwTypeError t_x t_y = lift $ throwTypeError t_x t_y
#endif

#ifndef HLINT
  default throwRowError :: (MonadTrans t, Unify v m)
                        => Label -> Type (v (Node Type v)) -> t m a
  throwRowError l c_r = lift $ throwRowError l c_r
#endif

instance Unify v m => Unify v (ReaderT r m)

unify :: Unify v m => v (Node Type v) -> v (Node Type v) -> m ()
unify v_x v_y = whenM (v_x /== v_y) $ do
  n_x <- v_x^!contents
  let v_k_x = n_x^.kind
  n_y <- v_y^!contents
  let v_k_y = n_y^.kind
  Kind.unify v_k_x v_k_y
  case (n_x^.term, n_y^.term) of
    (Bot, Bot) -> merge v_x v_y
    (_, Bot) -> graft v_x v_y
    (Bot, _) -> graft v_y v_x
    (Const c, Const c') | c == c'-> merge v_x v_y
    (t_x, t_y) ->
      switch (v_k_x, t_x)
      $ caseM (first (contents.term._Row)._2.extension).:
      (\ ((v_l_t_x, l), v_r_x) -> do
        (v_l_t_y, v_r_y) <- withoutTailOf v_x $ getRow l v_y
        merge v_x v_y
        unify v_l_t_x v_l_t_y
        unify v_r_x v_r_y)
      $ default' $ case (t_x, t_y) of
        (App t1 t2, App t1' t2') -> do
          merge v_x v_y
          unify t1 t1'
          unify t2 t2'
        _ -> throwTypeError t_x t_y

getRow :: (Unify v m, MonadReader (v (Node Type v)) m)
       => Label
       -> v (Node Type v)
       -> m (v (Node Type v), v (Node Type v))
getRow l v_r0 = v_r0^!contents >>= \ n_r0 ->
  let c_r0 = n_r0^.term in
  switch c_r0
  $ case' _Bot.: (\ () -> do
    whenM (isTailOf v_r0 =<< ask) $
      throwTypeError c_r0 =<< perform (contents.term) =<< ask
    withBindingOf n_r0 $ do
      v_r1 <- bot row
      v_l_t1 <- app (extend l) (bot star) (row ~> row)
      join $ graft <$> app (pure v_l_t1) (pure v_r1) row <*> pure v_r0
      return (v_l_t1, v_r1))
  $ caseM extension.: (\ ((v_l_t1, l'), v_r1) ->
    if l == l'
    then return (v_l_t1, v_r1)
    else do
      (v_l_t2, v_r2) <- getRow l v_r1
      (v_l_t2, ) <$> withBindingOf n_r0 (app (pure v_l_t1) (pure v_r2) row))
  $ default' $ throwRowError l c_r0

withoutTailOf :: f (Node Type f) -> ReaderT (f (Node Type f)) m a -> m a
withoutTailOf = flip runReaderT

isTailOf :: MonadUnionFind f m => f (Node Type f) -> f (Node Type f) -> m Bool
v_x `isTailOf` v_y =
  switch v_y
  $ caseM (contents.term._App._2).:
  (\ v_ys -> v_x === v_ys)
  $ default' $ return False

withBindingOf :: MonadUnionFind v m
              => Node Type v
              -> ReaderT (BindingFlag, Binder Type v) m a
              -> m a
withBindingOf n m =
  runReaderT m =<<
  n^!duplicated.
  first (bindingFlag.contents).
  second (binder.contents)

extension :: (Applicative f, Effective m r f, MonadUnionFind var m)
          => LensLike' f
             (Type (var (Node Type var)))
             ((var (Node Type var), Label), var (Node Type var))
extension = _App.first (duplicated.second (contents.term._App._1.
                                           contents.term._Const._Extend))
