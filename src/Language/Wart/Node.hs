{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Node (IsNode (..)) where

import Control.Lens

class IsNode s b v | s -> b v, b -> s v, v -> s b where
  binding :: Getter s b
  value :: Getter s v
