{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Node (IsNode (..)) where

import Control.Lens

class IsNode s b v | s -> b, s -> v where
  binding :: Getter s b
  value :: Getter s v
