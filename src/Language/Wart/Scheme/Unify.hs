{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
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
import qualified Control.Lens.Iso.Generics as Iso
import Control.Lens.Switch
import Control.Lens.Tuple.Extras
import qualified Control.Lens.Tuple.Generics as Tuple
import Control.Monad.Extras
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.UnionFind
import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Data.Foldable (Foldable, foldl', for_, toList, traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.LCA.Online (Path, lca)
import qualified Data.LCA.Online.Extras as Path
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Semigroup (Semigroup ((<>)), mempty)
import Data.Semigroup.Foldable
import Data.Traversable (for)
import GHC.Generics (Generic)
import Language.Wart.BindingFlag
import Language.Wart.Graphic
import Language.Wart.Kind (Kind)
import qualified Language.Wart.Kind as Kind
import Language.Wart.Scheme.Graphic (Scheme)
import qualified Language.Wart.Scheme.Graphic as Scheme
import Language.Wart.Type (Type, Label, kind)
import qualified Language.Wart.Type as Type
import Type.Nat

class (MonadSupply Int m, MonadUnionFind v m) => Unify v m | m -> v where
  throwKindError :: Kind (v (Node Kind v)) -> Kind (v (Node Kind v)) -> m a
  throwTypeError :: Type (v (Node Type v)) -> Type (v (Node Type v)) -> m a
  throwRowError :: Label -> Type (v (Node Type v)) -> m a
  throwCyclicTypeError :: v (Node Type v) -> m a
  throwCyclicKindError :: v (Node Kind v) -> m a
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
  default throwCyclicTypeError :: (MonadTrans t, Unify v m)
                               => v (Node Type v) -> t m a
  throwCyclicTypeError v_t = lift $ throwCyclicTypeError v_t
#endif

#ifndef HLINT
  default throwCyclicKindError :: (MonadTrans t, Unify v m)
                               => v (Node Kind v) -> t m a
  throwCyclicKindError v_k = lift $ throwCyclicKindError v_k
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

instance Unify v m => Unify v (ReaderT r m)
instance Unify v m => Unify v (StateT s m)

unify :: Unify v m => v (Node Type v) -> v (Node Type v) -> m ()
unify v_x v_y = do
  Two v_x0 v_y0 <- cloneTypes $ Two v_x v_y
  
  s <- execUnifierT $ Type.unify v_x v_y

  checkCycles v_x

  for_ (s^.mergedBindingFlags&mapped.mapped %~ view _3) $ updateBindingFlags

  (b2_t, b2_k) <- getVirtualBinders v_x (s^.graftedNonBots)

  updateBinders
    v_x
    (s^.mergedTypeBinders&mapped.mapped %~ view _3)
    (s^.mergedKindBinders&mapped.mapped %~ view _3)
    b2_t
    b2_k

  runCheckerT (do
    for_ (s^.graftedBots)
      checkGraft

    forOf_ (folded.folded) (s^.mergedTypeBinders) $ \ (MergedBinder i b v_b) ->
      join $ checkRaise i <$> b^!label <*> v_b^!contents.label
    forOf_ (folded.folded) (s^.mergedKindBinders) $ \ (MergedBinder i b v_b) ->
      join $ checkRaise i <$> b^!label <*> v_b^!contents.label

    forOf_ (folded.folded) (s^.mergedBindingFlags) $ \ (MergedBindingFlag i bf v_bf) ->
      checkWeaken i bf =<< v_bf^!contents

    checkMerges =<<
      (<>) <$>
      (for (s^.mergedTypes) $ \ v_t ->
        v_t^!contents.(label &&& binder.contents.label).from tupled) <*>
      (for (s^.mergedKinds) $ \ v_k ->
        v_k^!contents.(label &&& binder.contents.label).from tupled))

    v_x0
    v_y0
    (s^.permissions)

#ifndef HLINT
cloneTypes :: (Traversable t, MonadUnionFind v m)
           => t (v (Node Type v)) -> m (t (v (Node Type v)))
cloneTypes = runTypeCloner . traverse cloneType
  where
    cloneType = perform contents >=> \ n_t -> do
      use (_1.at (n_t^.label)) >>= \ case
        Just v_t' -> return v_t'
        Nothing -> do
          v_bf' <- new =<< n_t^!bindingFlag.contents
          v_b' <- n_t^!binder.contents >>= (\ case
            Type.Scheme s ->
              return $ _Scheme#s
            Type.Type v_t -> do
              i_t <- v_t^!contents.label
              maybe (_Type#v_t) (_Type#) <$> use (_1.at i_t)) >>= new
          t' <- traverse cloneType (n_t^.term)
          v_k' <- cloneKind $ n_t^.kind
          v_t' <- new $ n_t
            &bindingFlag .~ v_bf'
            &binder .~ v_b'
            &term .~ t'
            &kind .~ v_k'
          _1.at (n_t^.label) <?= v_t'
    cloneKind = perform contents >=> \ n_k -> do
      use (_2.at (n_k^.label)) >>= \ case
        Just v_k' -> return v_k'
        Nothing -> do
          v_bf' <- new =<< n_k^!bindingFlag.contents
          v_b' <- n_k^!binder.contents >>= (\ case
            Kind.Scheme s ->
              return $ _Scheme#s
            Kind.Type v_t -> do
              i_t <- v_t^!contents.label
              maybe (_Type#v_t) (_Type#) <$> use (_1.at i_t)
            Kind.Kind v_k -> do
              i_k <- v_k^!contents.label
              maybe (_Kind#v_k) (_Kind#) <$> use (_2.at i_k)) >>= new
          k' <- traverse cloneKind (n_k^.term)
          v_k' <- new $ n_k
            &bindingFlag .~ v_bf'
            &binder .~ v_b'
            &term .~ k'
          _2.at (n_k^.label) <?= v_k'
    runTypeCloner = flip evalStateT (IntMap.empty, IntMap.empty)
#endif

checkCycles :: Unify v m => v (Node Type v) -> m ()
checkCycles = runCycleCheckerT . checkTypeCycles
  where
    checkTypeCycles v_t = do
      n_t <- v_t^!contents
      let i_t = n_t^.label
      use (at i_t) >>= \ case
        Nothing -> do
          at i_t ?= True
          traverse_ checkTypeCycles (n_t^.term)
          at i_t ?= False
          checkKindCycles $ n_t^.kind
        Just True -> throwCyclicTypeError v_t
        Just False -> return ()
    checkKindCycles v_k = do
      n_k <- v_k^!contents
      let i_k = n_k^.label
      use (at i_k) >>= \ case
        Nothing -> do
          at i_k ?= True
          traverse_ checkKindCycles (n_k^.term)
          at i_k ?= False
        Just True -> throwCyclicKindError v_k
        Just False -> return ()
    runCycleCheckerT = flip evalStateT IntMap.empty

updateBindingFlags :: MonadUnionFind v m
                   => Foldable t => t (v BindingFlag) -> m ()
updateBindingFlags v_bfs = for2_ v_bfs $ \ v_bf_x v_bf_y -> do
  bf_x <- v_bf_x^!contents
  bf_y <- v_bf_y^!contents
  union v_bf_x v_bf_y
  write v_bf_x $! max bf_x bf_y

getVirtualBinders :: MonadUnionFind v m
                  => v (Node Type v) -> IntSet -> m (VirtualBinders v)
getVirtualBinders = execVirtualBindersT . putVirtualTypeBinders
  where
    putVirtualTypeBinders v_t = do
      n_t <- v_t^!contents
      let i_t = n_t^.label
      unlessM (use $ _1.contains i_t) $ do
        _1.contains i_t .= True
        view (contains i_t) >>= \ case
          True -> do
            for_ (n_t^.term) $ putPartiallyGraftedType (_Type#v_t)
            putPartiallyGraftedKind (_Type#v_t) (n_t^.kind)
          False -> do
            for_ (n_t^.term) putVirtualTypeBinders
            n_t^.kind&putVirtualKindBinders
    putVirtualKindBinders v_k = do
      n_k <- v_k^!contents
      let i_k = n_k^.label
      unlessM (use $ _2.contains i_k) $ do
        _2.contains i_k .= True
        view (contains i_k) >>= \ case
          True -> for_ (n_k^.term) $ putPartiallyGraftedKind (_Kind#v_k)
          False -> for_ (n_k^.term) putVirtualKindBinders
    putPartiallyGraftedType b v_t = do
      n_t <- v_t^!contents
      let i_t = n_t^.label
      whenNothingM (_3.at i_t <<%= Just . maybe (Bag.singleton b) (Bag.insert b)) $ do
        for_ (n_t^.term) $ putPartiallyGraftedType (_Type#v_t)
        n_t^.kind&putPartiallyGraftedKind (_Type#v_t)
    putPartiallyGraftedKind b v_k = do
      n_k <- v_k^!contents
      let i_k = n_k^.label
      whenNothingM (_4.at i_k <<%= Just . maybe (Bag.singleton b) (Bag.insert b)) $
        for_ (n_k^.term) $ putPartiallyGraftedKind (_Kind#v_k)
    execVirtualBindersT m =
      fmap (\ (_, _, b2_t, b2_k) -> (b2_t, b2_k)) .
      runReaderT (execStateT m (IntSet.empty,
                                IntSet.empty,
                                IntMap.empty,
                                IntMap.empty))
      

updateBinders :: MonadUnionFind v m
              => v (Node Type v)
              -> IntMap (Bag (v (Binder Type v)))
              -> IntMap (Bag (v (Binder Kind v)))
              -> IntMap (Bag (Binder Type v))
              -> IntMap (Bag (Binder Kind v))
              -> m ()
updateBinders = runBinderUpdater . updateTypeBinders
  where
    updateTypeBinders v_t = do
      n_t <- v_t^!contents
      let (i_t, v_b_t) = n_t^.(label &&& binder)
      b1 <- view _1
      b1_n <- join $
              forOf (traversed.traversed) (b1^.at i_t) <$>
              pure (\ v_b_t' -> do
        p <- getTypePath =<< v_b_t'^!contents
        p <$ union v_b_t v_b_t')
      b2 <- view _3
      b2_n <- getTypePath $ b2^.at i_t
      whenJust (b1_n <> b2_n) $ \ ps -> do
        let p' = getLCA $ foldMap1 LCA ps
        putTypePath i_t p'
        write v_b_t $ Path.head p'
      for_ (n_t^.term) updateTypeBinders
      n_t^.kind&updateKindBinders
    updateKindBinders v_k = do
      n_k <- v_k^!contents
      let (i_k, v_b_k) = n_k^.(label &&& binder)
      b1 <- view _2
      b1_n <- join $
              forOf (traversed.traversed) (b1^.at i_k) <$>
              pure (\ v_b_k' -> do
        p <- getKindPath =<< v_b_k'^!contents
        p <$ union v_b_k v_b_k')
      b2 <- view _4
      b2_n <- getKindPath $ b2^.at i_k
      whenJust (b1_n <> b2_n) $ \ ps -> do
        let p' = getLCA $ foldMap1 LCA ps
        putKindPath i_k p'
        write v_b_k $ Path.head p'
      for_ (n_k^.term) updateKindBinders
    getTypePath = undefined
    getKindPath = undefined
    putTypePath i_t p = _1.at i_t ?= p
    putKindPath i_k p = _2.at i_k ?= p
    runBinderUpdater m b1_t b1_k b2_t b2_k =
      flip runReaderT (b1_t, b1_k, b2_t, b2_k) $
      evalStateT m (IntMap.empty, IntMap.empty)

newtype LCA a = LCA { getLCA :: Path a }

instance Semigroup (LCA a) where
  x <> y = LCA $ lca (getLCA x) (getLCA y)

#ifndef HLINT
checkGraft :: Unify v m => Int -> CheckerT v m ()
checkGraft i = view (permissions.at i) >>= \ case
  Just G -> return ()
  _ -> join $ throwGraftError i <$> view _1 <*> view _2
#endif

#ifndef HLINT
checkRaise :: Unify v m => Int -> Int -> Int -> CheckerT v m ()
checkRaise i i_b i_b' = view (permissions.at i) >>= \ case
  Just R -> when (i_b /= i_b') $ join $ throwRaiseError i <$> view _1 <*> view _2
  _ -> return ()
#endif

#ifndef HLINT
checkWeaken :: Unify v m
            => Int
            -> BindingFlag
            -> BindingFlag
            -> CheckerT v m ()
checkWeaken i bf bf' = view (permissions.at i) >>= \ case
  Just R -> when (bf /= bf') $ join $ throwWeakenError i <$> view _1 <*> view _2
  _ -> return ()
#endif

checkMerges :: Unify v m => IntMap MergedBinding -> CheckerT v m ()
checkMerges bs =
  view _1 >>= \ v_x0 ->
  view _2 >>= \ v_y0 ->
  join $
  runMergeCheckerT (for_ (Two v_x0 v_y0) checkTypeMerges) v_x0 v_y0 <$>
  view permissions
  where
    checkTypeMerges v0 = do
      n0 <- v0^!contents
      let i0 = n0^.label
      unlessM (use $ _1.contains i0) $ do
        _1.contains i0 .= True
        whenJust (bs^.at i0) $ \ (MergedBinding i i') -> do
          i0' <- fromMaybe i' <$> view (_4.at i')
          v_x0 <- view _1
          v_y0 <- view _2
          use (_2.at (i, i0')) >>= \ case
            Nothing -> view (_3.at i0) >>= (_2.at (i, i0') ?=) . \ case
              Just R -> Just i0
              _ -> Nothing
            Just (Just j0) -> throwMergeError j0 v_x0 v_y0
            Just Nothing -> view (_3.at i0) >>= \ case
              Just R -> throwMergeError i0 v_x0 v_y0
              _ -> return ()
          local (_4.at i ?~ i0) $ do
            for_ (n0^.term) checkTypeMerges
            n0^.kind&checkKindMerges
    checkKindMerges v0 = do
      n0 <- v0^!contents
      let i0 = n0^.label
      unlessM (use $ _1.contains i0) $ do
        _1.contains i0 .= True
        whenJust (bs^.at i0) $ \ (MergedBinding i i') -> do
          i0' <- fromMaybe i' <$> view (_4.at i')
          v_x0 <- view _1
          v_y0 <- view _2
          ps <- view _3
          use (_2.at (i, i0')) >>= \ case
            Nothing -> view (_3.at i0) >>= (_2.at (i, i0') ?=) . \ case
              Just R -> Just i0
              _ -> Nothing
            Just (Just j0) -> throwMergeError j0 v_x0 v_y0
            Just Nothing -> view (_3.at i0) >>= \ case
              Just R -> throwMergeError i0 v_x0 v_y0
              _ -> return ()
          local (_4.at i ?~ i0) $ for_ (n0^.term) checkKindMerges
    runMergeCheckerT m v_x0 v_y0 ps =
      flip runReaderT (v_x0, v_y0, ps, IntMap.empty) $
      evalStateT m (IntSet.empty, HashMap.empty)

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

instance MonadSupply s m => MonadSupply s (IdT m)

instance MonadUnionFind v m => MonadUnionFind v (IdT m)

instance Unify v m => Unify v (IdT m)

type UnifierT v m = IdT (StateT (UnifierState v) m)

instance Unify v m => Type.Unify v (UnifierT v m) where
  throwTypeError t_x t_y = lift $ throwTypeError t_x t_y
  throwRowError l t = lift $ throwRowError l t
  merge = undefined
  graft = undefined

instance Unify v m => Kind.Unify v (UnifierT v m) where
  throwKindError k_x k_y = lift $ throwKindError k_x k_y
  merge = undefined
  graft = undefined

data UnifierState v =
  UnifierState
  (MergedTypes v)
  (MergedKinds v)
  (MergedBindingFlags v)
  (MergedTypeBinders v)
  (MergedKindBinders v)
  GraftedNonBots
  GraftedBots
  Colors
  Morphisms deriving Generic

instance (Functor f, Contravariant f) => HasPermission f (UnifierState v) where
  permissions = undefined

type MergedTypes v = IntMap (v (Node Type v))
type MergedKinds v = IntMap (v (Node Kind v))
type MergedBindingFlags v = IntMap (Bag (MergedBindingFlag v))
type MergedTypeBinders v = IntMap (Bag (MergedBinder Type v))
type MergedKindBinders v = IntMap (Bag (MergedBinder Kind v))
type GraftedNonBots = IntSet
type GraftedBots = [Int]

data MergedBindingFlag v =
  MergedBindingFlag
  {-# UNPACK #-} !Int
  !BindingFlag
  (v BindingFlag) deriving Generic
instance Field1
         (MergedBindingFlag v) (MergedBindingFlag v)
         Int Int
instance Field2
         (MergedBindingFlag v) (MergedBindingFlag v)
         BindingFlag BindingFlag
instance Field3
         (MergedBindingFlag v) (MergedBindingFlag v)
         (v BindingFlag) (v BindingFlag)

data MergedBinder c v =
  MergedBinder
  {-# UNPACK #-} !Int
  (Binder c v)
  (v (Binder c v)) deriving Generic
instance Field1
         (MergedBinder c v) (MergedBinder c v)
         Int Int
instance Field2
         (MergedBinder c v) (MergedBinder c v)
         (Binder c v) (Binder c v)
instance Field3
         (MergedBinder c v) (MergedBinder c v)
         (v (Binder c v)) (v (Binder c v))

mergedTypes :: Lens' (UnifierState v) (MergedTypes v)
mergedTypes = Tuple.ix (Proxy :: Proxy N0)

mergedKinds :: Lens' (UnifierState v) (MergedKinds v)
mergedKinds = Tuple.ix (Proxy :: Proxy N1)

mergedBindingFlags :: Lens' (UnifierState v) (MergedBindingFlags v)
mergedBindingFlags = Tuple.ix (Proxy :: Proxy N2)

mergedTypeBinders :: Lens' (UnifierState v) (MergedTypeBinders v)
mergedTypeBinders = Tuple.ix (Proxy :: Proxy N3)

mergedKindBinders :: Lens' (UnifierState v) (MergedKindBinders v)
mergedKindBinders = Tuple.ix (Proxy :: Proxy N4)

graftedNonBots :: Lens' (UnifierState v) GraftedNonBots
graftedNonBots = Tuple.ix (Proxy :: Proxy N5)

graftedBots :: Lens' (UnifierState v) GraftedBots
graftedBots = Tuple.ix (Proxy :: Proxy N6)

execUnifierT :: Monad m => UnifierT v m a -> m (UnifierState v)
execUnifierT =
  flip execStateT (UnifierState
                   mempty
                   mempty
                   mempty
                   mempty
                   mempty
                   mempty
                   mempty
                   mempty
                   mempty) .
  runIdT

type RebinderT c v m = IdT (StateT (RebinderState c v) m)

newtype RebinderState c v =
  RebinderState
  (IntMap (Path (Binder c v))) deriving Generic

paths :: Iso' (RebinderState c v) (IntMap (Path (Binder c v)))
paths = from Iso.wrapped

runRebinderT :: Monad m => RebinderT c v m a -> m a
runRebinderT = flip evalStateT (RebinderState mempty) . runIdT

type CheckerT v m = IdT (ReaderT (CheckerEnv v) m)

data CheckerEnv v =
  CheckerEnv
  (v (Node Type v))
  (v (Node Type v))
  Permissions deriving Generic
instance Field1
         (CheckerEnv v) (CheckerEnv v)
         (v (Node Type v)) (v (Node Type v))
instance Field2
         (CheckerEnv v) (CheckerEnv v)
         (v (Node Type v)) (v (Node Type v))

instance Functor f => HasPermission f (CheckerEnv v) where
  permissions = Tuple.ix (Proxy :: Proxy N2)

runCheckerT :: CheckerT v m a
            -> v (Node Type v)
            -> v (Node Type v)
            -> Permissions
            -> m a
runCheckerT m v_x v_y ps = runReaderT (runIdT m) (CheckerEnv v_x v_y ps)

data MergedBinding =
  MergedBinding
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int deriving Generic
instance Tuple MergedBinding (Int, Int)

type Permissions = IntMap Permission

class Functor f => HasPermission f s where
  permissions :: LensLike' f s Permissions

type VirtualBinders v =
  (IntMap (Bag (Binder Type v)), IntMap (Bag (Binder Kind v)))

data Permission = M | I | G | O | R deriving Eq

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

data Two a = Two a a deriving (Functor, Foldable, Traversable, Generic)
instance Tuple (Two a) (a, a)

infix 4 <%?=, ?<>=

(<%?=) :: (Profunctor p, MonadState s m)
       => Over p ((,) b) s s (Maybe a) (Maybe b) -> p (Maybe a) b -> m b
{-# INLINE (<%?=) #-}
l <%?= f = l %%= rmap (\ b -> (b, Just b)) f

(?<>=) :: (MonadState s m, Semigroup a) => ASetter' s (Maybe a) -> a -> m ()
{-# INLINE (?<>=) #-}
l ?<>= b = modify (l %~ Just . maybe b (<> b))
