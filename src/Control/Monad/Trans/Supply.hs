module Control.Monad.Trans.Supply
       ( Supply
       , runSupply
       , SupplyT
       , runSupplyT
       , supply
       ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Data.Stream

type Supply s = SupplyT s Identity

runSupply :: Supply s a -> Stream s -> a
runSupply m = runIdentity . runSupplyT m

newtype SupplyT s m a = SupplyT { unSupplyT :: StateT (Stream s) m a }

instance Functor m => Functor (SupplyT s m) where
  fmap f = SupplyT . fmap f . unSupplyT

instance (Functor m, Monad m) => Applicative (SupplyT s m) where
  pure = SupplyT . pure
  f <*> a = SupplyT $ unSupplyT f <*> unSupplyT a

instance Monad m => Monad (SupplyT s m) where
  return = SupplyT . return
  m >>= f = SupplyT $ unSupplyT m >>= unSupplyT . f
  fail = SupplyT . fail

runSupplyT :: Monad m => SupplyT s m a -> Stream s -> m a
runSupplyT = evalStateT . unSupplyT

supply :: Monad m => SupplyT s m s
supply = SupplyT $ do
  x :| xs <- get
  put xs
  return x
