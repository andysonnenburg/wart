{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.UnionFind.Class
       ( MonadUnionFind (..)
       , contents
       ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.ST.Safe
import qualified Control.Monad.UnionFind.ST as ST
import Prelude hiding (read)

infix 4 ===, /==

class (Applicative m, Monad m) => MonadUnionFind f m | m -> f where
  new :: a -> m (f a)
  read :: f a -> m a
  write :: f a -> a -> m ()
  union :: f a -> f a -> m ()
  unionWith :: (a -> a -> a) -> f a -> f a -> m ()
  (===) :: f a -> f a -> m Bool
  (/==) :: f a -> f a -> m Bool

#ifndef HLINT
  default new :: (MonadTrans t, MonadUnionFind f m) => a -> t m (f a)
  new = lift . new
#endif

#ifndef HLINT
  default read :: (MonadTrans t, MonadUnionFind f m) => f a -> t m a
  read = lift . read
#endif

#ifndef HLINT
  default write :: (MonadTrans t, MonadUnionFind f m) => f a -> a -> t m ()
  write x = lift . write x
#endif

#ifndef HLINT
  default union :: (MonadTrans t, MonadUnionFind f m) => f a -> f a -> t m ()
  union x y = lift $ union x y
#endif

#ifndef HLINT
  default unionWith :: (MonadTrans t, MonadUnionFind f m)
                    => (a -> a -> a) -> f a -> f a -> t m ()
  unionWith f x y = lift $ unionWith f x y
#endif

#ifndef HLINT
  default (===) :: (MonadTrans t, MonadUnionFind f m) => f a -> f a -> t m Bool
  x === y = lift $ x === y
#endif

#ifndef HLINT
  default (/==) :: (MonadTrans t, MonadUnionFind f m) => f a -> f a -> t m Bool
  x /== y = lift $ x /== y
#endif

contents :: MonadUnionFind f m => IndexPreservingAction m (f a) a
{-# INLINE contents #-}
contents = act read

instance MonadUnionFind (ST.Var s) (ST s) where
  new = ST.new
  read = ST.read
  write = ST.write
  union = ST.union
  unionWith = ST.unionWith
  (===) = (ST.===)
  (/==) = (ST./==)

instance MonadUnionFind f m => MonadUnionFind f (ReaderT r m)
