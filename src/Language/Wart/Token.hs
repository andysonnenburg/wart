{-# LANGUAGE DeriveGeneric #-}
module Language.Wart.Token (Token (..)) where

import Data.Text (Text)
import GHC.Generics (Generic)

data Token
  = Var
  | Fn
  | For
  | While
  | LeftBrace
  | RightBrace
  | LeftParen
  | RightParen
  | Semicolon
  | Name {-# UNPACK #-} !Name
  | EOF deriving (Show, Generic)

type Name = Text
