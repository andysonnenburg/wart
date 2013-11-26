{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.BindingFlag
       ( BindingFlag (..)
       , flexible, rigid
       ) where

import Control.Lens
import Control.Lens.Union
import GHC.Generics (Generic)

data BindingFlag = Flexible | Rigid deriving (Show, Generic)
instance VariantA BindingFlag BindingFlag () ()
instance VariantB BindingFlag BindingFlag () ()

flexible :: Prism' BindingFlag ()
flexible = _A

rigid :: Prism' BindingFlag ()
rigid = _B
