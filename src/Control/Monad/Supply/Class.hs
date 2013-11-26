{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Supply.Class
       ( MonadSupply (..)
       ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Supply (SupplyT)
import qualified Control.Monad.Trans.Supply as Trans

class (Applicative m, Monad m) => MonadSupply s m | m -> s where
  supply :: m s

#ifndef HLINT
  default supply :: (MonadTrans t, MonadSupply s m) => t m s
  supply = lift supply
#endif

instance (Applicative m, Monad m) => MonadSupply s (SupplyT s m) where
  supply = Trans.supply

instance MonadSupply s m => MonadSupply s (ReaderT r m)
