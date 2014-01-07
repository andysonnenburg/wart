{-# LANGUAGE DeriveGeneric #-}
module Language.Wart.Scheme.Internal (Scheme (..)) where

import GHC.Generics (Generic)

data Scheme a = G deriving Generic
