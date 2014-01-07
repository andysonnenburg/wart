module Control.Lens.Action.Extras (perform_) where

import Control.Lens
import Control.Lens.Internal.Action
import Data.Profunctor.Unsafe

perform_ :: Monad m => Acting m () s a -> s -> m ()
perform_ l = getEffect #. l (Effect #. const (return ()))
