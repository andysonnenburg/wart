{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Scheme.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Action.Extras
import Control.Lens.Extras
import Control.Lens.Internal.Action (Effect)
import Control.Lens.Switch
import Control.Lens.Tuple.Extras
import qualified Control.Lens.Tuple.Generics as Tuple
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.UnionFind
import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Data.Foldable (Foldable, for_)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import Data.LCA.Online (Path)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Semigroup (Semigroup ((<>)), mempty)
import GHC.Generics (Generic)
import Language.Wart.BindingFlag
import Language.Wart.Graphic
import Language.Wart.Kind (Kind)
import Language.Wart.Scheme.Graphic (Scheme)
import qualified Language.Wart.Scheme.Graphic as Scheme
import Language.Wart.Type (Type, Label, kind)
import qualified Language.Wart.Type as Type
import Type.Nat

class (MonadSupply Int m, MonadUnionFind v m) => Unify v m | m -> v where
  throwKindError :: Kind (v (Node Kind v)) -> Kind (v (Node Kind v)) -> m a
  throwTypeError :: Type (v (Node Type v)) -> Type (v (Node Type v)) -> m a
  throwRowError :: Label -> Type (v (Node Type v)) -> m a
  throwGraftError :: Int -> v (Node Type v) -> v (Node Type v) -> m a
  throwRaiseError :: Int -> v (Node Type v) -> v (Node Type v) -> m a
  throwWeakenError :: Int -> v (Node Type v) -> v (Node Type v) -> m a
  throwMergeError :: Int -> v (Node Type v) -> v (Node Type v) -> m a

#ifndef HLINT
  default throwKindError :: (MonadTrans t, Unify v m)
                         =>  Kind (v (Node Kind v)) -> Kind (v (Node Kind v)) -> t m b
  throwKindError c_x c_y = lift $ throwKindError c_x c_y
#endif

#ifndef HLINT
  default throwTypeError :: (MonadTrans t, Unify v m)
                         => Type (v (Node Type v)) -> Type (v (Node Type v)) -> t m a
  throwTypeError c_x c_y = lift $ throwTypeError c_x c_y
#endif

#ifndef HLINT
  default throwRowError :: (MonadTrans t, Unify v m)
                        => Label -> Type (v (Node Type v)) -> t m a
  throwRowError l c = lift $ throwRowError l c
#endif

#ifndef HLINT
  default throwGraftError :: (MonadTrans t, Unify v m)
                          => Int -> v (Node Type v) -> v (Node Type v) -> t m a
  throwGraftError i v_x v_y = lift $ throwGraftError i v_x v_y
#endif

#ifndef HLINT
  default throwRaiseError :: (MonadTrans t, Unify v m)
                          => Int -> v (Node Type v) -> v (Node Type v) -> t m a
  throwRaiseError i v_x v_y = lift $ throwRaiseError i v_x v_y
#endif

#ifndef HLINT
  default throwWeakenError :: (MonadTrans t, Unify v m)
                           => Int -> v (Node Type v) -> v (Node Type v) -> t m a
  throwWeakenError i v_x v_y = lift $ throwWeakenError i v_x v_y
#endif

#ifndef HLINT
  default throwMergeError :: (MonadTrans t, Unify v m)
                          => Int -> v (Node Type v) -> v (Node Type v) -> t m a
  throwMergeError i v_x v_y = lift $ throwMergeError i v_x v_y
#endif

unify :: Unify v m => v (Node Type v) -> v (Node Type v) -> m ()
unify v_x v_y = do
  Two v_x0 v_y0 <- cloneTypes $ Two v_x v_y
  
  s <- execUnifierT $ Type.unify v_x v_y

  checkCycles v_x

  forOf_ _3 (s^.mergedBindingFlags) updateBindingFlags

  (b2_t, b2_k) <- getVirtualBinders v_x (s^.graftedNonBots)

  updateBinders
    v_x
    (sunion (s^.mergedTypeBinders&traversed.traversed %~ view _3) b2_t)
    (sunion (s^.mergedKindBinders&traversed.traversed %~ view _3) b2_k)

  runCheckerT (do
    for_ (s^.graftedBots)
      checkGraft
    forOf_ (folded.folded) (s^.mergedTypeBinders) $ \ (i, b, v_b) ->
      checkTypeRaise i b v_b
    forOf_ (folded.folded) (s^.mergedKindBinders) $ \ (i, b, v_b) ->
      checkKindRaise i b v_b
    forOf_ (folded.folded) (s^.mergedBindingFlags) $ \ (i, bf, v_bf) ->
      checkWeaken i bf v_bf)
    (s^.permissions)
    v_x0
    v_y0

#ifndef HLINT
cloneTypes :: (Traversable t, MonadUnionFind v m)
           => t (v (Node Type v)) -> m (t (v (Node Type v)))
cloneTypes =
  flip evalStateT (IntMap.empty, IntMap.empty) .
  traverse cloneType
  where
    cloneType v_t = do
      n_t <- v_t^!contents
      use (_1.at (n_t^.label)) >>= \ case
        Just v_t' -> return v_t'
        Nothing -> do
          v_bf' <- new =<< n_t^!bindingFlag.contents
          v_b' <- n_t^!binder.contents >>= (\ case
            Type.Scheme s ->
              return $ _Scheme#s
            Type.Type v_t' -> do
              i_t' <- v_t'^!contents.label
              review _Type <$> use (_1.to (!i_t'))) >>= new
          v_k' <- cloneKind $ n_t^.kind
          v_t' <- new $ n_t
            &bindingFlag .~ v_bf'
            &binder .~ v_b'
            &kind .~ v_k'
          _1.at (n_t^.label) <?= v_t'
    cloneKind = undefined
#endif

getVirtualBinders :: v (Node Type v) -> IntSet -> m (VirtualBinders v)
getVirtualBinders = undefined

newtype IdT m a = IdT { runIdT :: m a }

instance Functor m => Functor (IdT m) where
  fmap f = IdT . fmap f . runIdT

instance Applicative m => Applicative (IdT m) where
  pure = IdT . pure
  f <*> a = IdT $ runIdT f <*> runIdT a

instance Monad m => Monad (IdT m) where
  return = IdT . return
  m >>= f = IdT $ runIdT m >>= runIdT . f
  fail = IdT . fail

instance MonadTrans IdT where
  lift = IdT

instance MonadReader r m => MonadReader r (IdT m) where
  ask = lift ask
  local f = lift . local f . runIdT

instance MonadUnionFind v m => MonadUnionFind v (IdT m)

type UnifierT v m = IdT (StateT (UnifierState v) m)

data UnifierState v =
  UnifierState
  (MergedBindingFlags v)
  (MergedTypeBinders v)
  (MergedKindBinders v)
  GraftedNonBots
  GraftedBots
  Colors
  Morphisms deriving Generic

type MergedBindingFlags v = IntMap (Bag (Int, BindingFlag, v BindingFlag))
type MergedTypeBinders v = IntMap (Bag (Int, Binder Type v, v (Binder Type v)))
type MergedKindBinders v = IntMap (Bag (Int, Binder Kind v, v (Binder Kind v)))
type GraftedNonBots = IntSet
type GraftedBots = [Int]

mergedBindingFlags :: Lens' (UnifierState v) (MergedBindingFlags v)
mergedBindingFlags = Tuple.ix (Proxy :: Proxy N0)

mergedTypeBinders :: Lens' (UnifierState v) (MergedTypeBinders v)
mergedTypeBinders = Tuple.ix (Proxy :: Proxy N1)

mergedKindBinders :: Lens' (UnifierState v) (MergedKindBinders v)
mergedKindBinders = Tuple.ix (Proxy :: Proxy N2)

graftedNonBots :: Lens' (UnifierState v) GraftedNonBots
graftedNonBots = Tuple.ix (Proxy :: Proxy N3)

graftedBots :: Lens' (UnifierState v) GraftedBots
graftedBots = Tuple.ix (Proxy :: Proxy N4)

execUnifierT :: Monad m => UnifierT v m a -> m (UnifierState v)
execUnifierT =
  flip execStateT (UnifierState mempty mempty mempty mempty mempty mempty mempty) .
  runIdT

type CheckerT v m = IdT (ReaderT (CheckerEnv v) m)

data CheckerEnv v =
  CheckerEnv
  (v (Node Type v))
  (v (Node Type v))
  Permissions deriving Generic
instance Field1 (CheckerEnv v) (CheckerEnv v) (v (Node Type v)) (v (Node Type v))
instance Field2 (CheckerEnv v) (CheckerEnv v) (v (Node Type v)) (v (Node Type v))

runCheckerT :: CheckerT v m a
            -> Permissions
            -> v (Node Type v)
            -> v (Node Type v)
            -> m a
runCheckerT m ps v_x v_y = runReaderT (runIdT m) (CheckerEnv v_x v_y ps)

type Permissions = IntMap Permission

class HasPermission s where
  permissions :: Lens' s Permissions

checkCycles :: MonadUnionFind v m => v (Node Type v) -> m ()
checkCycles = undefined

updateBindingFlags :: MonadUnionFind v m
                   => Foldable t => t (v BindingFlag) -> RebinderT m ()
updateBindingFlags v_bfs = for2_ v_bfs $ \ v_bf_x v_bf_y -> do
  bf_x <- v_bf_x^!contents
  bf_y <- v_bf_y^!contents
  union v_bf_x v_bf_y
  write v_bf_x $! max bf_x bf_y

updateBinders :: v (Node Type v)
              -> IntMap (Bag (Binder Type v))
              -> IntMap (Bag (Binder Kind v))
              -> RebinderT m ()
updateBinders = undefined

#ifndef HLINT
checkGraft :: (Applicative m, Monad m) => Int -> CheckerT v m ()
checkGraft i = view (permissions.at i) >>= \ case
  Just G -> return ()
  _ -> join $ throwGraftError i <$> view _1 <*> view _2
#endif

#ifndef HLINT
checkTypeRaise :: MonadUnionFind v m
               => Int
               -> Binder Type v
               -> v (Binder Type v)
               -> CheckerT v m ()
checkTypeRaise i b v_b = view (permissions.at i) >>= \ case
  _ -> return ()
  Just R -> do
    b' <- v_b^!contents
    switch (b, b')
      $ case' (first (_Scheme.label).second (_Scheme.label)).:
      (\ (i_b, i_b') -> when (i_b /= i_b') $ throwRaiseError')
      $ caseM (first (_Type.contents.label).second (_Type.contents.label)).:
      (\ (i_b, i_b') -> when (i_b /= i_b') $ throwRaiseError')
      $ default' $ throwRaiseError'
  where
    throwRaiseError' = join $ throwRaiseError i <$> view _1 <*> view _2
#endif

#ifndef HLINT
checkKindRaise :: MonadUnionFind v m
               => Int
               -> Binder Kind v
               -> v (Binder Kind v)
               -> CheckerT v m ()
checkKindRaise i b v_b = view (permissions.at i) >>= \ case
  _ -> return ()
  Just R -> do
    b' <- v_b^!contents
    switch (b, b')
      $ case' (first (_Scheme.label).second (_Scheme.label)).:
      (\ (i_b, i_b') -> when (i_b /= i_b') $ throwRaiseError')
      $ caseM (first (_Type.contents.label).second (_Type.contents.label)).:
      (\ (i_b, i_b') -> when (i_b /= i_b') $ throwRaiseError')
      $ caseM (first (_Kind.contents.label).second (_Kind.contents.label)).:
      (\ (i_b, i_b') -> when (i_b /= i_b') $ throwRaiseError')
      $ default' $ throwRaiseError'
  where
    throwRaiseError' = join $ throwRaiseError i <$> view _1 <*> view _2
#endif

#ifndef HLINT
checkWeaken :: MonadUnionFind v m
            => Int
            -> BindingFlag
            -> v BindingFlag
            -> CheckerT v m ()
checkWeaken i bf v_bf = view (permissions.at i) >>= \ case
  _ -> return ()
  Just R -> do
    bf' <- v_bf^!contents
    when (bf /= bf') $ join $ throwWeakenError i <$> view _1 <*> view _2
#endif

type RebinderT m a = m a

runRebinderT :: RebinderT m a -> m a
runRebinderT = id

type VirtualBinders v = (IntMap (Bag (Binder Type v)), IntMap (Bag (Binder Kind v)))

type Paths v = (IntMap (Path (Node Kind v)), IntMap (Path (Node Type v)))

data Permission = M | I | G | O | R

type Colors = IntMap Color

data Color = Green | Orange | Red

type Morphisms = IntMap Morphism

data Morphism = Polymorphic | Monomorphic | Inert

instance Semigroup Morphism where
  Polymorphic <> _ = Polymorphic
  _ <> Polymorphic = Polymorphic
  Inert <> _ = Inert
  _ <> Inert = Inert
  Monomorphic <> Monomorphic = Monomorphic

sunion :: Semigroup a => IntMap a -> IntMap a -> IntMap a
sunion = IntMap.unionWith (<>)

#ifndef HLINT
for2_ :: (Applicative f, Monad f, Foldable t) => t a -> (a -> a -> f b) -> f ()
for2_ xs f = flip evalStateT Nothing $ for_ xs $ \ x -> get >>= \ case
  Just y -> lift (f y x) *> put (Just x)
  Nothing -> return ()
#endif

data Two a = Two a a deriving (Functor, Foldable, Traversable)

infix 4 <%?=, ?<>=

(<%?=) :: (Profunctor p, MonadState s m)
       => Over p ((,) b) s s (Maybe a) (Maybe b) -> p (Maybe a) b -> m b
{-# INLINE (<%?=) #-}
l <%?= f = l %%= rmap (\ b -> (b, Just b)) f

(?<>=) :: (MonadState s m, Semigroup a) => ASetter' s (Maybe a) -> a -> m ()
{-# INLINE (?<>=) #-}
l ?<>= b = modify (l %~ Just . maybe b (<> b))
