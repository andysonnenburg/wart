{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.BindingFlag
       ( BindingFlag (..), _Flexible, _Rigid
       ) where

import Control.Lens
import qualified Control.Lens.Union.Generics as Union
import Data.Proxy
import GHC.Generics (Generic)
import Type.Nat

data BindingFlag = Flexible | Rigid deriving (Show, Eq, Ord, Generic)

_Flexible :: Prism' BindingFlag ()
_Flexible = Union.ix (Proxy :: Proxy N0)

_Rigid :: Prism' BindingFlag ()
_Rigid = Union.ix (Proxy :: Proxy N1)
