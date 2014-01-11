module Control.Monad.Extras
       ( whenM
       , whenJust
       ) where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
{-# INLINE whenM #-}
whenM p m = p >>= flip when m

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
{-# INLINE whenJust #-}
whenJust p m = maybe (return ()) m p
