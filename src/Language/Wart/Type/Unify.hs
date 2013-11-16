{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Type.Unify
       ( Unify (..)
       , unify
       ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Extras
import Control.Monad.UnionFind
import Language.Wart.Kind (Kinded (..))
import qualified Language.Wart.Kind as Kind
import Language.Wart.Type.Syntax

class Unify f m where
  typeMismatch :: f (Type f) -> f (Type f) -> m ()
  merged :: f (Type f) -> f (Type f) -> m ()
  grafted :: f (Type f) -> f (Type f) -> m ()

unify :: Unify f m => f (Type f) -> f (Type f) -> m ()
unify f_x f_y = whenM (f_x /== f_y) $ do
  Kind.unify (f_x^.kind) (f_y^.kind)

