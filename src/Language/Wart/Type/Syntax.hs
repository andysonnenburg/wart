{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Type.Syntax
       ( Type (..)
       , bot
       , fn
       , number
       , string
       , record
       , variant
       , app
       , Typed (..)
       ) where

import Control.Lens
import Control.Lens.Union
import GHC.Generics (Generic)

data Type f
  = Bot
  | Fn {-# UNPACK #-} !Arity
  | Number
  | String
  | Record
  | Variant
  | App (f (Type f)) (f (Type f)) deriving Generic

type Arity = Int

instance VariantA (Type f) (Type f) () ()
instance VariantB (Type f) (Type f) Arity Arity
instance VariantC (Type f) (Type f) () ()
instance VariantD (Type f) (Type f) () ()
instance VariantE (Type f) (Type f) () ()
instance VariantF (Type f) (Type f) () ()
instance VariantG (Type f) (Type f') (f (Type f), f (Type f)) (f' (Type f'), f' (Type f'))

bot :: Prism' (Type f) ()
bot = _A

fn :: Prism' (Type f) Arity
fn = _B

number :: Prism' (Type f) ()
number = _C

string :: Prism' (Type f) ()
string = _D

record :: Prism' (Type f) ()
record = _E

variant :: Prism' (Type f) ()
variant = _F

app :: Prism (Type f) (Type f') (f (Type f), f (Type f)) (f' (Type f'), f' (Type f'))
app = _G

class Typed f a | a -> f where
  type' :: Getter a (f (Type f))
