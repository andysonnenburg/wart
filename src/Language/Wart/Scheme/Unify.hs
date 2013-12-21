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

import Control.Applicative
import Control.Lens
import Control.Lens.Action.Extras
import Control.Lens.Extras
import Control.Lens.Internal.Action (Effect)
import Control.Lens.Tuple.Extras
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.UnionFind
import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.LCA.Online (Path)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as IntMap
import Data.Semigroup (Semigroup ((<>)), mempty)
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
  throwRebindError :: f (Type.Node f) -> f (Type.Node f) -> Int -> Int -> m a

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
  default throwRebindError :: (MonadTrans t, Unify f m)
                           => f (Type.Node f)
                           -> f (Type.Node f)
                           -> Int
                           -> Int
                           -> t m a
  throwRebindError v_x v_y i_x i_y = lift $ throwRebindError v_x v_y i_x i_y
#endif

unify :: Unify f m
      => f (Type.Node f)
      -> f (Type.Node f)
      -> m ()
unify v_x v_y = do
  v_x0 <- cloneType v_x
  v_y0 <- cloneType v_y
  s <- execUnifierT $ Type.unify v_x v_y
  checkCycles v_x v_y
  join $
    runRebinderT (rebind v_x v_y) (s^.merged) <$>
    getVirtualBinders (s^.grafted.kinds) (s^.grafted.types) <*>
    pure (s^.permissions)
  runCheckerT (do
    checkGrafts $ s^.grafted.bots
    checkWeakens $ s^.merged.kinds
    checkWeakens $ s^.merged.types
    checkRaises $ s^.merged.kinds
    checkRaises $ s^.merged.types
    checkMerges $ s^.merged.kinds
    checkMerges $ s^.merged.types) v_x0 v_y0 (s^.permissions)

rebind :: Monad m => f (Type.Node f) -> f (Type.Node f) -> RebinderT f m ()
rebind = undefined

instance Unify f m => Kind.Unify f (UnifierT f m) where
  throwKindError v_x v_y = lift $ throwKindError v_x v_y
  merge v_x v_y = do
    n_x <- v_x^!contents
    n_y <- v_y^!contents
    m_y <- merged.kinds.at (n_y^.int) <<.= Nothing <&> fromMaybe (Bag.singleton n_y)
    merged.kinds.at (n_x^.int) %= Just . maybe (Bag.insert n_x m_y) (flip Bag.union m_y)
    tellPermissionOf n_x
    tellPermissionOf n_y
    union v_x v_y
  graft v_x v_y = do
    n_x <- v_x^!contents
    grafted.kinds %= (v_x:)
    n_y <- v_y^!contents
    grafted.bots %= (n_y^.int:)
    Kind.merge v_x v_y

instance Unify f m => Type.Unify f (UnifierT f m) where
  throwTypeError v_x v_y = lift $ throwTypeError v_x v_y
  throwRowError l v_r = lift $ throwRowError l v_r
  merge v_x v_y = do
    n_x <- v_x^!contents
    n_y <- v_y^!contents
    m_y <- merged.types.at (n_y^.int) <<.= Nothing <&> fromMaybe (Bag.singleton n_y)
    merged.types.at (n_x^.int) %= Just . maybe (Bag.insert n_x m_y) (flip Bag.union m_y)
    tellPermissionOf n_x
    tellPermissionOf n_y
    union v_x v_y
  graft v_x v_y = do
    n_x <- v_x^!contents
    grafted.types %= (v_x:)
    n_y <- v_y^!contents
    grafted.bots %= (n_y^.int:)
    Type.merge v_x v_y

type VirtualBinders f = (IntMap (Kind.Node f), IntMap (Type.Node f))

type Paths f = (IntMap (Path (Kind.Node f)), IntMap (Path (Type.Node f)))

type Permissions = IntMap Permission

data Permission = M | I | G | O | R

tellPermissionOf :: (Monad m,
                     HasMorphism (Effect (UnifierT var m) ()) s,
                     HasColor (Effect (UnifierT var m) ()) s)
                 => s -> UnifierT var m ()
tellPermissionOf s = do
  perform_ color s
  perform_ morphism s

#ifndef HLINT
permissions :: Colors -> Morphisms -> Permissions
permissions = IntMap.intersectionWith $ curry $ \ case
  (_, Monomorphic) -> M
  (_, Inert) -> I
  (Green, _) -> G
  (Orange, _) -> O
  (Red, _) -> R
#endif

type Colors = IntMap Color

data Color = Green | Orange | Red

colors :: Field3 s t Colors Colors => Lens s t Colors Colors
colors = _3

type Morphisms = IntMap Morphism

data Morphism = Polymorphic | Monomorphic | Inert

instance Semigroup Morphism where
  Polymorphic <> _ = Polymorphic
  _ <> Polymorphic = Polymorphic
  Inert <> _ = Inert
  _ <> Inert = Inert
  Monomorphic <> Monomorphic = Monomorphic

morphisms :: Field4 s t Morphisms Morphisms => Lens s t Morphisms Morphisms
morphisms = _4

type Merged f = (IntMap (Bag (Kind.Node f)), IntMap (Bag (Type.Node f)))

merged :: Field1 s t (Merged f) (Merged f) => Lens s t (Merged f) (Merged f)
merged = _1

type Grafted f = ([f (Kind.Node f)], [f (Type.Node f)], [Int])

grafted :: Field2 s t (Grafted f) (Grafted f) => Lens s t (Grafted f) (Grafted f)
grafted = _2

kinds :: Field1 s t a b => Lens s t a b
kinds = _1

types :: Field2 s t a b => Lens s t a b
types = _2

bots :: Field3 s t [Int] [Int] => Lens s t [Int] [Int]
bots = _3

class Functor f => HasColor f s where
  color :: LensLike' f s Color

instance (Effective (UnifierT var m) r f, MonadUnionFind var m) =>
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

instance (Effective (UnifierT var m) r f, MonadUnionFind var m)
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

class Functor f => HasMorphism f s where
  morphism :: LensLike' f s Morphism

instance (Effective (UnifierT var m) r f, MonadUnionFind var m)
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

instance (Effective (UnifierT var m) r f, MonadUnionFind var m)
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

instance (Effective (UnifierT var m) r f, MonadUnionFind var m)
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

newtype UnifierT f m a =
  UnifierT { unUnifierT :: StateT (Merged f,
                                   Grafted f,
                                   Colors,
                                   Morphisms) m a
           }

execUnifierT :: Monad m => UnifierT f m a -> m (Merged f, Grafted f, Permissions)
execUnifierT m = do
  s <- execStateT (unUnifierT m) mempty
  return (s^.merged, s^.grafted, permissions (s^.colors) (s^.morphisms))

instance Functor m => Functor (UnifierT f m) where
  fmap f = UnifierT . fmap f . unUnifierT

instance (Functor m, Monad m) => Applicative (UnifierT f m) where
  pure = UnifierT . pure
  f <*> a = UnifierT $ unUnifierT f <*> unUnifierT a

instance Monad m => Monad (UnifierT f m) where
  return = UnifierT . return
  m >>= f = UnifierT $ unUnifierT m >>= unUnifierT . f
  fail = UnifierT . fail

instance MonadTrans (UnifierT f) where
  lift = UnifierT . lift

instance Monad m => MonadState (Merged f,
                                Grafted f,
                                Colors,
                                Morphisms) (UnifierT f m) where
  state = UnifierT . state
  get = UnifierT get
  put = UnifierT . put

instance MonadSupply s m => MonadSupply s (UnifierT f m)
instance MonadUnionFind f m => MonadUnionFind f (UnifierT f m)

type RebinderT f m = ReaderT (Merged f,
                              VirtualBinders f,
                              Permissions) (StateT (Paths f) m)

runRebinderT :: RebinderT f m a
             -> Merged f
             -> VirtualBinders f
             -> Permissions
             -> m a
runRebinderT m ms vbs ps = evalStateT (runReaderT m ms vbs ps) mempty

type CheckerT = ReaderT (f (Type.Node f), f (Type.Node f), Ancestors)

type Ancestors = IntMap IntSet

runCheckerT :: CheckerT m a -> m a
runCheckerT m v_x v_y = do
  as <- runStateT (fix $ \ rec v -> do
    )
  runReaderT m (v_x, v_y, as)

cloneType :: f (Type.Node f) -> m (f (Type.Node f))
cloneType = evalStateT (fix $ \ rec v -> do
  n <- v^!contents
  let i = n^.int
  use (types.contains i) >>= \ case
    Just v' -> return v'
    Nothing -> do
      v' <- new n
      types.at i ?= v'
      v_b' <- new =<< n^!binding.contents
      t' <- traverse rec $ n^.value
      v_k' <- cloneKind $ n^.kind
      write v' $ Type.Node i v_b' t' v_k'
      return v') mempty
  where
    cloneKind v = do
      n <- v^!contents
      let i = n^.int
      use (kinds.contains i) >>= \ case
        Just v' -> return v'
        Nothing -> do
          v' <- new n
          kinds.at i ?= v'
          v_b' <- new =<< n^!binding.contents
          t' <- traverse rec $ n^.value
          v_k' <- cloneKind $ n^.kind
          write v' $ Kind.Node i v_b' t' v_k'
          return v'

infix 4 <%?=, ?<>=

(<%?=) :: (Profunctor p, MonadState s m)
       => Over p ((,) b) s s (Maybe a) (Maybe b) -> p (Maybe a) b -> m b
{-# INLINE (<%?=) #-}
l <%?= f = l %%= rmap (\ b -> (b, Just b)) f

(?<>=) :: (MonadState s m, Semigroup a) => ASetter' s (Maybe a) -> a -> m ()
{-# INLINE (?<>=) #-}
l ?<>= b = modify (l %~ Just . maybe b (<> b))
