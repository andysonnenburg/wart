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
import Control.Lens ((^.), (^!), (%~), _2, re, to)
import Control.Monad.Extras
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.Trans.Maybe
import Control.Monad.UnionFind
import Language.Wart.Binding
import Language.Wart.Kind (throwKindError)
import qualified Language.Wart.Kind as Kind
import Language.Wart.Node
import Language.Wart.Type.Syntax (Const (..), Label, Type (Bot), cloneBinder, kind)
import qualified Language.Wart.Type.Syntax as Type
import Prelude (Bool (..), Int, Maybe (..), ($), (.), (==), otherwise, undefined)

class (MonadSupply Int m, Kind.Unify f m) => Unify f m where
  merged :: Type.Node f -> Type.Node f -> m ()
  grafted :: Type.Node f -> Type.Node f -> m ()
  throwTypeError :: Type (f (Type.Node f)) -> Type (f (Type.Node f)) -> m b
  throwRowError :: Label -> Type (f (Type.Node f)) -> m b

#ifndef HLINT
  default merged :: (MonadTrans t, Unify f m) => Type.Node f -> Type.Node f -> t m ()
  merged n_x n_y = lift $ merged n_x n_y
#endif

#ifndef HLINT
  default grafted :: (MonadTrans t, Unify f m) => Type.Node f -> Type.Node f -> t m ()
  grafted n_x n_y = lift $ grafted n_x n_y
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
  (,) <$> getUnifiable (n_x^.value) <*> getUnifiable (n_y^.value) >>= \ case
    (Var, Var) -> do
      Kind.unify (n_x^.kind) (n_y^.kind)
      merged n_x n_y
      union v_x v_y
    (_, Var) -> do
      Kind.unify (n_x^.kind) (n_y^.kind)
      merged n_x n_y
      grafted n_x n_y
      union v_x v_y
    (Var, _) -> do
      Kind.unify (n_x^.kind) (n_y^.kind)
      merged n_y n_x
      grafted n_y n_x
      union v_y v_x
    (Const c, Const c')
      | c == c'-> do
        merged n_x n_y
        union v_x v_y
      | otherwise -> throwTypeError (Type.Const c) (Type.Const c')
    (App t1 t2, App t1' t2') -> do
      join $ Kind.unify <$> t1^!contents.kind <*> t1'^!contents.kind
      merged n_x n_y
      union v_x v_y
      unify t1 t1'
      unify t2 t2'
    (Row l v_t v_r, _) -> do
      (v_t', v_s') <- runReaderT (unifyRow l v_y) v_r
      merged n_x n_y
      unify v_t v_t'
      unify v_r v_s'
    _ -> join $ throwKindError <$> n_x^!kind.contents.value <*> n_y^!kind.contents.value
  union (n_x^.binding) (n_y^.binding)
#endif

#ifndef HLINT
unifyRow :: (Unify f m, MonadReader (f (Type.Node f)) m)
         => Label -> f (Type.Node f) -> m (f (Type.Node f), f (Type.Node f))
unifyRow l v_r = read v_r >>= \ n_r -> getUnifiable (n_r^.value) >>= \ case
  Const Empty -> throwRowError l $ Type.Const Empty
  Row l' v_t' v_r'
    | l == l' -> return (v_t', v_r')
    | otherwise -> undefined
  Var -> do
    v_r' <- ask
    whenM (v_r `isTailOf` v_r') $ throwTypeError (n_r^.value) =<< v_r'^!contents.value
    let v_b_t = n_r^.binding
    v_b_k <- new =<< v_b_t^!contents.tupled.to (_2 %~ cloneBinder).re tupled
    v_g <- new =<< Type.newNode v_b_t Bot =<< new =<< Kind.newNode v_b_k Kind.Star
    v_b <- new =<< Type.newNode v_b_t Bot =<< new =<< Kind.newNode v_b_k Kind.Row
    return (v_g, v_b)
  _ -> throwKindError Kind.Row =<< n_r^!kind.contents.value
#endif

data Unifiable a
  = Const !Const
  | Var
  | App a a
  | Row {-# UNPACK #-} !Label a a

getUnifiable :: Unify f m => Type (f (Type.Node f)) -> m (Unifiable (f (Type.Node f)))
getUnifiable = undefined

isTailOf :: MonadUnionFind f m => f (Type.Node f) -> f (Type.Node f) -> m Bool
isTailOf = undefined
