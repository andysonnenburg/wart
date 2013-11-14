module Control.Monad.Extras
       ( whenM
       ) where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= flip when m
