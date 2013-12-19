{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.BindingFlag
       ( BindingFlag (..), _Flexible, _Rigid
       ) where

import Control.Lens
import Control.Lens.Union
import GHC.Generics (Generic)

data BindingFlag = Flexible | Rigid deriving (Show, Generic)
instance VariantA BindingFlag BindingFlag () ()
instance VariantB BindingFlag BindingFlag () ()

_Flexible :: Prism' BindingFlag ()
_Flexible = _A

_Rigid :: Prism' BindingFlag ()
_Rigid = _B
