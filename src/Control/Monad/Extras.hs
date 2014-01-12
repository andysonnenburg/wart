module Control.Monad.Extras
       ( whenM
       , unlessM
       , whenJust
       ) where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
{-# INLINE whenM #-}
whenM p m = p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
{-# INLINE unlessM #-}
unlessM p m = p >>= flip unless m

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
{-# INLINE whenJust #-}
whenJust p m = maybe (return ()) m p
