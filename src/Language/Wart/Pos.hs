{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Pos (Pos (..)) where

import Control.Lens
import GHC.Generics (Generic)

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Generic)
instance Field1 Pos Pos Int Int
instance Field2 Pos Pos Int Int
