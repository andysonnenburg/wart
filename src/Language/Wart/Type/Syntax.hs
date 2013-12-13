{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Wart.Type.Syntax
       ( Type (..)
       , _Bot, _Const, _App, bot, const', app
       , Const (..)
       , _Number, _String, _Fn, _Record, _Variant, _Empty, _Extend
       , number, string, fn, record, variant, empty, extend
       , Arity
       , Label
       , Binding (..), bindingFlag, binder
       , Binder (..), _Scheme, IsBinder (..), cloneBinder
       , Node (..), kind
       ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Lens (Choice,
                     Field1 (..),
                     Field2 (..),
                     Field3 (..),
                     Field4 (..),
                     Lens',
                     Magnify (..),
                     Optic',
                     Prism,
                     Prism',
                     Profunctor (..),
                     (^.),
                     (%~),
                     re,
                     to)
import Control.Lens.Union
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.UnionFind
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity)
import Data.Tagged (Tagged)
import Data.Text (Text)
import Data.Traversable (Traversable, sequenceA)
import GHC.Generics (Generic)
import Language.Wart.Binding
import Language.Wart.BindingFlag
import {-# SOURCE #-} Language.Wart.Kind.Syntax ((~>), row, star)
import {-# SOURCE #-} qualified Language.Wart.Kind.Syntax as Kind
import Language.Wart.Node
import {-# SOURCE #-} Language.Wart.Scheme.Syntax (_Scheme)
import {-# SOURCE #-} qualified Language.Wart.Scheme.Syntax as Scheme

data Type a
  = Bot
  | Const Const
  | App a a deriving (Functor, Foldable, Traversable, Generic)
instance VariantA (Type a) (Type a) () ()
instance VariantB (Type a) (Type a) Const Const
instance VariantC (Type a) (Type a') (a, a) (a', a')

_Bot :: Prism' (Type a) ()
_Bot = _A

_Const :: Prism' (Type a) Const
_Const = _B

_App :: Prism (Type a) (Type a') (a, a) (a', a')
_App = _C

bot :: (MonadSupply Int m, MonadUnionFind f m)
    => ReaderT (Kind.Binding f) m (f (Kind.Node f))
    -> ReaderT (Binding f) m (f (Node f))
bot = type' Bot

const' :: (MonadSupply Int m, MonadUnionFind f m)
       => Const
       -> ReaderT (Kind.Binding f) m (f (Kind.Node f))
       -> ReaderT (Binding f) m (f (Node f))
const' = type' . Const

app :: (MonadSupply Int m, MonadUnionFind f m)
    => ReaderT (Binding f) m (f (Node f))
    -> ReaderT (Binding f) m (f (Node f))
    -> ReaderT (Kind.Binding f) m (f (Kind.Node f))
    -> ReaderT (Binding f) m (f (Node f))
app m_f m_a = type' $ App m_f m_a

type' :: (MonadSupply Int m, MonadUnionFind f m)
      => Type (ReaderT (Binding f) m (f (Node f)))
      -> ReaderT (Kind.Binding f) m (f (Kind.Node f))
      -> ReaderT (Binding f) m (f (Node f))
type' c m_k =
  new <=< join $
  newNode <$>
  (new =<< ask) <*>
  sequenceA c <*>
  magnify (tupled.to (_2 %~ cloneBinder).re tupled) m_k

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

_Number :: Prism' Const ()
_Number = _A

_String :: Prism' Const ()
_String = _B

_Fn :: Prism' Const Arity
_Fn = _C

_Record :: Prism' Const ()
_Record = _D

_Variant :: Prism' Const ()
_Variant = _E

_Empty :: Prism' Const ()
_Empty = _F

_Extend :: Prism' Const Label
_Extend = _G

number :: (MonadSupply Int m, MonadUnionFind f m)
       => ReaderT (Binding f) m (f (Node f))
number = const' Number star

string :: (MonadSupply Int m, MonadUnionFind f m)
       => ReaderT (Binding f) m (f (Node f))
string = const' String star

fn :: (MonadSupply Int m, MonadUnionFind f m)
   => Arity
   -> ReaderT (Binding f) m (f (Node f))
fn n = const' (Fn n) (foldr (~>) star (replicate n star))

record :: (MonadSupply Int m, MonadUnionFind f m)
       => ReaderT (Binding f) m (f (Node f))
record = const' Record (row ~> star)

variant :: (MonadSupply Int m, MonadUnionFind f m)
        => ReaderT (Binding f) m (f (Node f))
variant = const' Variant (row ~> star)

empty :: (MonadSupply Int m, MonadUnionFind f m)
      => ReaderT (Binding f) m (f (Node f))
empty = const' Empty row

extend :: (MonadSupply Int m, MonadUnionFind f m)
       => Label
       -> ReaderT (Binding f) m (f (Node f))
extend l = const' (Extend l) (star ~> row ~> row)

type Arity = Int
type Label = Text

data Binding f = Binding !BindingFlag !(Binder f) deriving Generic
instance Field1 (Binding f) (Binding f) BindingFlag BindingFlag
instance Field2 (Binding f) (Binding f') (Binder f) (Binder f')

instance (Profunctor p, Functor f) =>
         IsBinding p f (Binding a) (Binding a) (Binder a) where
  tupled = dimap (\ (Binding a b) -> (a, b)) (fmap $ uncurry Binding)

data Binder f
  = Scheme !(Scheme.Node f)
  | Type (f (Node f)) deriving Generic
instance VariantA (Binder f) (Binder f) (Scheme.Node f) (Scheme.Node f)
instance VariantB (Binder f) (Binder f) (f (Node f)) (f (Node f))

instance (Choice p, Applicative f) =>
         Scheme.IsBinder p f (Binder a) (Scheme.Node a) where
  _Scheme = _A

class (Choice p, Functor f) => IsBinder p f s a | s -> a where
  _Type :: Optic' p f s a

instance (Choice p, Applicative f) =>
         IsBinder p f (Binder a) (a (Node a)) where
  _Type = _B

#ifndef HLINT
cloneBinder :: (Scheme.IsBinder Tagged Identity s (Scheme.Node f),
                IsBinder Tagged Identity s (f (Node f)))
            => Binder f -> s
cloneBinder = \ case
  Scheme n_s -> n_s^.re _Scheme
  Type v_t -> v_t^.re _Type
#endif

data Node f =
  Node
  {-# UNPACK #-} !Int
  (f (Binding f))
  (Type (f (Node f)))
  (f (Kind.Node f)) deriving Generic
instance Field1 (Node f) (Node f) Int Int
instance Field2 (Node f) (Node f) (f (Binding f)) (f (Binding f))
instance Field3 (Node f) (Node f) (Type (f (Node f))) (Type (f (Node f)))
instance Field4 (Node f) (Node f) (f (Kind.Node f)) (f (Kind.Node f))

instance IsNode (Node f) (f (Binding f)) (Type (f (Node f))) where
  int = _1
  binding = _2
  value = _3

kind :: Lens' (Node f) (f (Kind.Node f))
kind = _4

newNode :: MonadSupply Int m
        => f (Binding f)
        -> Type (f (Node f))
        -> f (Kind.Node f)
        -> m (Node f)
newNode b t k = (\ x -> Node x b t k) <$> supply
