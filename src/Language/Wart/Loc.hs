{-# LANGUAGE DeriveGeneric #-}
module Language.Wart.Loc (Loc (..), Located (..)) where

import GHC.Generics (Generic)
import Language.Wart.Pos

data Loc = Loc {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos deriving (Show, Generic)

data Located a = Located {-# UNPACK #-} !Loc a deriving (Show, Generic)
