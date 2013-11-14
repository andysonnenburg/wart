{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Kind.Syntax
       ( Kind (..)
       , default'
       , star
       , row
       , constr
       , freeze
       ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Union
import Control.Monad
import Control.Monad.UnionFind
import Data.Function (fix)
import GHC.Generics (Generic)
import Prelude hiding (read)

data Kind f
  = Default
  | Star
  | Row
  | f (Kind f) :-> f (Kind f) deriving Generic

instance VariantA (Kind f) (Kind f) () ()
instance VariantB (Kind f) (Kind f) () ()
instance VariantC (Kind f) (Kind f) () ()
instance VariantD (Kind f) (Kind f') (f (Kind f), f (Kind f)) (f' (Kind f'), f' (Kind f'))

default' :: Prism' (Kind f) ()
default' = _A

star :: Prism' (Kind f) ()
star = _B

row :: Prism' (Kind f) ()
row = _C

constr :: Prism (Kind f) (Kind f') (f (Kind f), f (Kind f)) (f' (Kind f'), f' (Kind f'))
constr = _D

#ifndef HLINT
freeze :: MonadUnionFind f m => f (Kind f) -> m (Identity (Kind Identity))
freeze = fix $ \ rec -> read >=> fmap Identity . \ case
  Default -> return Star
  Star -> return Star
  Row -> return Row
  a :-> b -> (:->) <$> rec a <*> rec b
#endif
