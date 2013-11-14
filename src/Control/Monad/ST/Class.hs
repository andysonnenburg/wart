{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Monad.ST.Class (MonadST (..)) where

import Control.Applicative
import Control.Monad.ST.Safe
import Control.Monad.ST.Lazy.Safe (strictToLazyST)
import qualified Control.Monad.ST.Lazy.Safe as Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as Strict

class (Applicative m, Monad m) => MonadST m where
  type World m
  liftST :: ST (World m) a -> m a
#ifndef HLINT
  default liftST :: (MonadTrans t, MonadST m) => ST (World m) a -> t m a
  liftST = lift . liftST
#endif

instance MonadST (ST s) where
  type World (ST s) = s
  liftST = id

instance MonadST (Lazy.ST s) where
  type World (Lazy.ST s) = s
  liftST = strictToLazyST

instance MonadST IO where
  type World IO = RealWorld
  liftST = stToIO

instance MonadST m => MonadST (ReaderT r m) where
  type World (ReaderT r m) = World m

instance MonadST m => MonadST (Strict.StateT s m) where
  type World (Strict.StateT s m) = World m
