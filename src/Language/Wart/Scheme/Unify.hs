{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Scheme.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Lens
import Control.Lens.Tuple.Extras
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Trans.Class
import Control.Monad.UnionFind
import Data.IntMap.Strict (IntMap)
import Language.Wart.Binding
import Language.Wart.BindingFlag
import qualified Language.Wart.Kind as Kind
import Language.Wart.Node
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

type Unifier (f :: * -> *) = StateT (Colors, Morhpisms)

data Permission = M | I | G | O | R

class Functor f => HasPermission f s where
  permission :: LensLike' f s Permission

instance (Contravariant f, Functor f) => HasPermission f (Scheme.Node var) where
  permission = to $ const G

type Colors = IntMap Color

colors :: (Functor f, Field1 s t Colors Colors)
       => (Colors -> f Colors)
       -> s
       -> f t
colors = _1

type Morhpisms = IntMap Morphism

morphisms :: (Functor f, Field2 s t Morhpisms Morhpisms)
          => (Morhpisms -> f Morhpisms)
          -> s
          -> f t
morphisms = _2

data Color = Green | Orange | Red

class Functor f => HasColor f s where
  color :: LensLike' f s Color

instance (Effective (Unifier var m) r f, MonadUnionFind var m) =>
         HasColor f (Kind.Node var) where
  color = act $ \ n -> use (colors.at (n^.int)) >>= \ case
    Just c -> return c
    Nothing ->
      (colors.at (n^.int) <?=) =<<
      n^!binding.contents.tupled.second (act $ \ case
        Kind.Scheme s -> s^!color
        Kind.Type v_t -> v_t^!contents.color
        Kind.Kind v_k -> v_k^!contents.color).color

instance (Effective (Unifier var m) r f, MonadUnionFind var m) =>
         HasColor f (Type.Node var) where
  color = act $ \ n -> use (colors.at (n^.int)) >>= \ case
    Just c -> return c
    Nothing ->
      (colors.at (n^.int) <?=) =<<
      n^!binding.contents.tupled.second (act $ \ case
        Type.Scheme s -> s^!color
        Type.Type v_t -> v_t^!contents.color).color

instance (Contravariant f, Functor f) => HasColor f (Scheme.Node var) where
  color = to $ const Green

instance (Contravariant f, Functor f) => HasColor f (BindingFlag, Color) where
  color = to $ \ case
    (Flexible, Green) -> Green
    (Rigid, _) -> Orange
    (Flexible, _) -> Red

data Morphism = Polymorphic | Monomorphic | Inert

class Functor f => HasMorphism f s where
  morphism :: LensLike' f s Morphism
