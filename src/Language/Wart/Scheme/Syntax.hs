{-# LANGUAGE DeriveGeneric #-}
module Language.Wart.Scheme.Syntax (Scheme (..)) where

import GHC.Generics (Generic)

data Scheme a = G deriving Generic
