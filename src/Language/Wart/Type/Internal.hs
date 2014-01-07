{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Type.Internal
       ( Type (..)
       , Const (..)
       , Arity
       ) where

import Control.Lens.Union
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Language.Wart.Label

data Type a
  = Bot
  | Const !Const
  | App a a deriving (Functor, Foldable, Traversable, Generic)
instance VariantA (Type a) (Type a) () ()
instance VariantB (Type a) (Type a) Const Const
instance VariantC (Type a) (Type a') (a, a) (a', a')

data Const
  = Number
  | String
  | Fn {-# UNPACK #-} !Arity
  | Record
  | Variant
  | Empty
  | Extend {-# UNPACK #-} !Label deriving (Eq, Generic)
instance VariantA Const Const () ()
instance VariantB Const Const () ()
instance VariantC Const Const Arity Arity
instance VariantD Const Const () ()
instance VariantE Const Const () ()
instance VariantF Const Const () ()
instance VariantG Const Const Label Label

type Arity = Int
