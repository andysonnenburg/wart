{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Lens.Switch
       ( switch, case', caseM, (.:), default'
       ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Setter
import Control.Monad
import Data.Distributive
import Data.Monoid
import Data.Profunctor.Rep

infixr 4 .:

switch :: a -> (a -> b) -> b
{-# INLINE switch #-}
switch = flip ($)

case' :: Representable p
      => Getting (First a) s a
      -> Setter' (p s r) (p a r)
{-# INLINE case' #-}
case' a = \ k f ->
  pure . tabulate $ \ s ->
  maybe (rep f s) (rep . untainted . k . tabulate . const $ rep f s) $
  s^?a

caseM :: (Monad m, Representable p, Distributive (Rep p))
      => Acting m (Leftmost a) s a
      -> Setter' (p s (m r)) (p a (m r))
{-# INLINE caseM #-}
caseM a = \ k f ->
  pure . tabulate $ \ s ->
  fmap join .
  collectM (maybe (rep f s) (rep . untainted . k . tabulate . const $ rep f s)) $
  s^!?a

default' :: (a -> b) -> a -> b
{-# INLINE default' #-}
default' = id

(.:) :: ASetter' s a -> a -> s -> s
{-# INLINE (.:) #-}
(.:) = (.~)
