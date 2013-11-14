{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.UnionFind.ST
       ( Var
       , new
       , union
       , unionWith
       , (===), (/==)
       , read
       , write
       , contents
       ) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Lens
import Control.Monad
import Control.Monad.ST.Class
import Control.Monad.ST.Safe
import Data.Function (fix)
import Data.STRef
import Data.STIntRef
import GHC.Generics (Generic)
import Prelude hiding (elem, read)

infix 4 ===, /==

newtype Var s a = Var { unVar :: STRef s (Link s a) }

data Link s a
  = Repr {-# UNPACK #-} !(STIntRef s) {-# UNPACK #-} !(STRef s a)
  | Link {-# UNPACK #-} !(STRef s (Link s a))

new :: MonadST m => a -> m (Var (World m) a)
new =
  liftST <<<
  fmap Var . newSTRef <=<
  liftA2 Repr (newSTIntRef minBound) <<<
  newSTRef

union :: (MonadST m, s ~ World m) => Var s a -> Var s a -> m ()
{-# INLINE union #-}
union = unionWith const

#ifndef HLINT
unionWith :: (MonadST m, s ~ World m)
          => (a -> a -> a) -> Var s a -> Var s a -> m ()
unionWith f x y = liftST $ do
  Three xRankRef xRef xLinkRef <- x^!repr
  Three yRankRef yRef yLinkRef <- y^!repr
  when (xRef /= yRef) $
    compare <$> readSTIntRef xRankRef <*> readSTIntRef yRankRef >>= \ case
      LT -> do
        writeSTRef xLinkRef $ Link yLinkRef
        writeSTRef yRef =<< f <$> readSTRef xRef <*> readSTRef yRef
      EQ -> do
        modifySTIntRef xRankRef (+ 1)
        writeSTRef yLinkRef $ Link xLinkRef
        writeSTRef xRef =<< f <$> readSTRef xRef <*> readSTRef yRef
      GT -> do
        writeSTRef yLinkRef $ Link xLinkRef
        writeSTRef xRef =<< f <$> readSTRef xRef <*> readSTRef yRef
#endif

(===) :: (MonadST m, s ~ World m) => Var s a -> Var s a -> m Bool
x === y = liftST $ (==) <$> x^!repr._2 <*> y^!repr._2

(/==) :: (MonadST m, s ~ World m) => Var s a -> Var s a -> m Bool
x /== y = liftST $ (/=) <$> x^!repr._2 <*> y^!repr._2

repr :: IndexPreservingAction (ST s) (Var s a) (Three s a)
{-# INLINE repr #-}
repr = act find

write :: MonadST m => Var (World m) a -> a -> m ()
{-# INLINE write #-}
write var a = liftST $ var^!repr._2 >>= flip writeSTRef a

read :: MonadST m => Var (World m) a -> m a
{-# INLINE read #-}
read = liftST . perform (repr._2.act readSTRef)

contents :: MonadST m => IndexPreservingAction m (Var (World m) a) a
{-# INLINE contents #-}
contents = act read

#ifndef HLINT
find :: Var s a -> ST s (Three s a)
find = fix (\ rec linkRef -> readSTRef linkRef >>= \ case
  Repr rankRef r -> return $! Three rankRef r linkRef
  Link linkRef' -> do
    x <- rec linkRef'
    writeSTRef linkRef $ Link $ x^._3
    return x) . unVar
#endif

data Three s a =
  Three
  {-# UNPACK #-} !(STIntRef s)
  {-# UNPACK #-} !(STRef s a)
  {-# UNPACK #-} !(STRef s (Link s a)) deriving Generic

instance Field1 (Three s a) (Three s a) (STIntRef s) (STIntRef s)
instance Field2 (Three s a) (Three s a) (STRef s a) (STRef s a)
instance Field3 (Three s a) (Three s a) (STRef s (Link s a)) (STRef s (Link s a))
