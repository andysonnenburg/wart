{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Kind.Syntax
       ( Kind (..)
       , default'
       , star
       , row
       , Kinded (..)
       ) where

import Control.Lens
import Control.Lens.Union
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

class Kinded f a | a -> f where
  kind :: Getter a (f (Kind f))
