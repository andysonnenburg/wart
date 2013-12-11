{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Wart.Kind.Syntax
       ( Kind (..), _Bot, _Star, _Row, bot, star, row, (-->)
       , Binding (..), bindingFlag, binder
       , Binder (..), _Scheme, _Type, _Kind
       , Node (..)
       ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Union
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.UnionFind
import Data.Foldable (Foldable)
import Data.Traversable (Traversable, sequenceA)
import GHC.Generics (Generic)
import Language.Wart.Binding
import Language.Wart.BindingFlag
import Language.Wart.Node
import Language.Wart.Scheme.Syntax (_Scheme)
import qualified Language.Wart.Scheme.Syntax as Scheme
import {-# SOURCE #-} Language.Wart.Type.Syntax (_Type)
import {-# SOURCE #-} qualified Language.Wart.Type.Syntax as Type

infixr 9 :->, -->

data Kind a
  = Bot
  | Star
  | Row
  | a :-> a deriving (Functor, Foldable, Traversable, Generic)
instance VariantA (Kind a) (Kind a) () ()
instance VariantB (Kind a) (Kind a) () ()
instance VariantC (Kind a) (Kind a) () ()
instance VariantD (Kind a) (Kind a') (a, a) (a', a')

_Bot :: Prism' (Kind a) ()
_Bot = _A

_Star :: Prism' (Kind a) ()
_Star = _B

_Row :: Prism' (Kind a) ()
_Row = _C

bot :: (MonadSupply Int m, MonadUnionFind f m)
    => ReaderT (Binding f) m (f (Node f))
bot = kind Bot

star :: (MonadSupply Int m, MonadUnionFind f m)
     => ReaderT (Binding f) m (f (Node f))
star = kind Star

row :: (MonadSupply Int m, MonadUnionFind f m)
     => ReaderT (Binding f) m (f (Node f))
row = kind Row

(-->) :: (MonadSupply Int m, MonadUnionFind f m)
      => ReaderT (Binding f) m (f (Node f))
      -> ReaderT (Binding f) m (f (Node f))
      -> ReaderT (Binding f) m (f (Node f))
a --> b = kind $ a :-> b

kind :: (MonadSupply Int m, MonadUnionFind f m)
     => Kind (ReaderT (Binding f) m (f (Node f)))
     -> ReaderT (Binding f) m (f (Node f))
kind c = new <=< join $ newNode <$> (new =<< ask) <*> sequenceA c

data Binding f = Binding !BindingFlag !(Binder f) deriving Generic
instance Field1 (Binding f) (Binding f) BindingFlag BindingFlag
instance Field2 (Binding f) (Binding f') (Binder f) (Binder f')

instance (Profunctor p, Functor f) =>
         IsBinding p f (Binding a) (Binding a) (Binder a) where
  tupled = dimap (\ (Binding a b) -> (a, b)) (fmap $ uncurry Binding)

data Binder f
  = Scheme (Scheme.Node f)
  | Type (f (Type.Node f))
  | Kind (f (Node f)) deriving Generic
instance VariantA (Binder f) (Binder f) (Scheme.Node f) (Scheme.Node f)
instance VariantB (Binder f) (Binder f) (f (Type.Node f)) (f (Type.Node f))
instance VariantC (Binder f) (Binder f) (f (Node f)) (f (Node f))

instance (Choice p, Applicative f) =>
         Scheme.IsBinder p f (Binder a) (Scheme.Node a) where
  _Scheme = _A

instance (Choice p, Applicative f) =>
         Type.IsBinder p f (Binder a) (a (Type.Node a)) where
  _Type = _B

_Kind :: Prism' (Binder f) (f (Node f))
_Kind = _C

data Node f =
  Node
  {-# UNPACK #-} !Int
  (f (Binding f))
  (Kind (f (Node f))) deriving Generic
instance Field1 (Node f) (Node f) Int Int
instance Field2 (Node f) (Node f) (f (Binding f)) (f (Binding f))
instance Field3 (Node f) (Node f) (Kind (f (Node f))) (Kind (f (Node f)))

instance IsNode (Node f) (f (Binding f)) (Kind (f (Node f))) where
  binding = _2
  value = _3


newNode :: MonadSupply Int m
        => f (Binding f)
        -> Kind (f (Node f))
        -> m (Node f)
newNode b k = (\ x -> Node x b k) <$> supply
