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
import Control.Lens.Action.Extras
import Control.Lens.Extras
import Control.Lens.Tuple.Extras
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.UnionFind
import Data.IntMap.Strict (IntMap)
import Data.Semigroup (Semigroup ((<>)))
import Language.Wart.Binding
import Language.Wart.BindingFlag
import qualified Language.Wart.Kind as Kind
import Language.Wart.Node
import Language.Wart.Scheme.Syntax (type')
import qualified Language.Wart.Scheme.Syntax as Scheme
import Language.Wart.Type (Label, kind)
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

type Unifier (f :: * -> *) = StateT (Colors, Morphisms)

data Permission = M | I | G | O | R

#ifndef HLINT
permission :: (Effective (Unifier f m) r f,
               HasColor (First s f) s,
               HasMorphism (Second Color f) s)
           => LensLike' f s Permission
permission = duplicated.first color.second morphism.to (\ case
  (_, Monomorphic) -> M
  (_, Inert) -> I
  (Green, _) -> G
  (Orange, _) -> O
  (Red, _) -> R)
#endif

type Colors = IntMap Color

colors :: (Functor f, Field1 s t Colors Colors)
       => (Colors -> f Colors)
       -> s
       -> f t
colors = _1

type Morphisms = IntMap Morphism

morphisms :: (Functor f, Field2 s t Morphisms Morphisms)
          => (Morphisms -> f Morphisms)
          -> s
          -> f t
morphisms = _2

data Color = Green | Orange | Red

class Functor f => HasColor f s where
  color :: LensLike' f s Color

instance (Effective (Unifier var m) r f, MonadUnionFind var m) =>
         HasColor f (Kind.Node var) where
#ifndef HLINT
  color = act $ \ n -> use (colors.at (n^.int)) >>= \ case
    Just c -> return c
    Nothing ->
      (colors.at (n^.int) <?=) =<<
      n^!binding.contents.tupled.second (act $ \ case
        Kind.Scheme s -> s^!color
        Kind.Type v_t -> v_t^!contents.color
        Kind.Kind v_k -> v_k^!contents.color).color
#endif

instance (Effective (Unifier var m) r f, MonadUnionFind var m)
      => HasColor f (Type.Node var) where
#ifndef HLINT
  color = act $ \ n -> use (colors.at (n^.int)) >>= \ case
    Just c -> return c
    Nothing ->
      (colors.at (n^.int) <?=) =<<
      n^!binding.contents.tupled.second (act $ \ case
        Type.Scheme s -> s^!color
        Type.Type v_t -> v_t^!contents.color).color
#endif

instance (Contravariant f, Functor f) => HasColor f (Scheme.Node var) where
  color = to $ const Green

instance (Contravariant f, Functor f) => HasColor f (BindingFlag, Color) where
#ifndef HLINT
  color = to $ \ case
    (Flexible, Green) -> Green
    (Rigid, _) -> Orange
    (Flexible, _) -> Red
#endif

data Morphism = Polymorphic | Monomorphic | Inert

instance Semigroup Morphism where
  Polymorphic <> _ = Polymorphic
  _ <> Polymorphic = Polymorphic
  Inert <> _ = Inert
  _ <> Inert = Inert
  Monomorphic <> Monomorphic = Monomorphic

class Functor f => HasMorphism f s where
  morphism :: LensLike' f s Morphism

instance (Effective (Unifier var m) r f, MonadUnionFind var m)
      => HasMorphism f (Kind.Node var) where
#ifndef HLINT
  morphism = act $ \ n -> use (morphisms.at (n^.int)) >>= \ case
    Just m -> return m
    Nothing -> do
      perform_ (value.folded.contents.morphism) n
      m <- morphisms.at (n^.int) <%?= \ case
        _ | n^.value&is Kind._Bot -> Polymorphic
        Nothing -> Monomorphic
        Just m -> m
      (bf, i) <- n^!binding.contents.tupled.second (act $ \ case
        Kind.Scheme s -> s^!int
        Kind.Type v_t -> v_t^!contents.int
        Kind.Kind v_k -> v_k^!contents.int)
      morphisms.at i ?<>= (bf, m)^.morphism
      return m
#endif

instance (Effective (Unifier var m) r f, MonadUnionFind var m)
      => HasMorphism f (Type.Node var) where
#ifndef HLINT
  morphism = act $ \ n -> use (morphisms.at (n^.int)) >>= \ case
    Just m -> return m
    Nothing -> do
      perform_ (value.folded.contents.morphism) n
      perform_ (kind.contents.morphism) n
      m <- morphisms.at (n^.int) <%?= \ case
        _ | n^.value&is Type._Bot -> Polymorphic
        Nothing -> Monomorphic
        Just m -> m
      (bf, i) <- n^!binding.contents.tupled.second (act $ \ case
        Type.Scheme s -> s^!int
        Type.Type v_t -> v_t^!contents.int)
      morphisms.at i ?<>= (bf, m)^.morphism
      return m
#endif

instance (Effective (Unifier var m) r f, MonadUnionFind var m)
      => HasMorphism f (Scheme.Node var) where
#ifndef HLINT
  morphism = act $ \ n -> use (morphisms.at (n^.int)) >>= \ case
    Just m -> return m
    Nothing -> do
      perform_ (type'.contents.morphism) n
      m <- morphisms.at (n^.int) <%?= \ case
        Nothing -> Monomorphic
        Just m -> m
      n^!?binding.contents.Scheme._Binder._1.int >>= \ case
        Nothing -> return ()
        Just i -> morphisms.at i ?<>= m
      return m
#endif

instance (Contravariant f, Functor f)
      => HasMorphism f (BindingFlag, Morphism) where
#ifndef HLINT
  morphism = to $ \ case
    (_, Monomorphic) -> Monomorphic
    (Rigid, _) -> Inert
    (Flexible, m) -> m
#endif

infix 4 <%?=, ?<>=

(<%?=) :: (Profunctor p, MonadState s m)
       => Over p ((,) b) s s (Maybe a) (Maybe b) -> p (Maybe a) b -> m b
{-# INLINE (<%?=) #-}
l <%?= f = l %%= rmap (\ b -> (b, Just b)) f
                     

(?<>=) :: (MonadState s m, Semigroup a) => ASetter' s (Maybe a) -> a -> m ()
{-# INLINE (?<>=) #-}
l ?<>= b = modify (l %~ Just . maybe b (<> b))
