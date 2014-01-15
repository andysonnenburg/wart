module Data.LCA.Online.Extras
       ( head
       , LCA (..)
       ) where

import Data.LCA.Online
import Data.LCA.View
import Data.Semigroup
import Prelude (($), error)

head :: Path a -> a
head xs = case view xs of
  Root -> error "Data.LCA.Online.Extras.head: Root"
  Node _ x _ -> x

newtype LCA a = LCA { getLCA :: Path a }

instance Semigroup (LCA a) where
  x <> y = LCA $ lca (getLCA x) (getLCA y)
