{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Wart.Type.Graphic
       ( module Language.Wart.Graphic
       , module Language.Wart.Type.Syntax
       , Node (..)
       , Binder (..)
       , kind
       , bot
       , app
       , extend
       ) where

import Control.Applicative (Applicative, (<$>), (<*>))
#ifndef HLINT
import Control.Lens (Choice, Lens', (%~), (#), _1, _2, view)
#endif
import qualified Control.Lens.Tuple.Generics as Tuple
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.UnionFind
import Data.Functor.Identity
import Data.Proxy
import Data.Tagged
import Data.Traversable (sequenceA)
import GHC.Generics (Generic)
import Language.Wart.BindingFlag
import Language.Wart.Graphic
import Language.Wart.Kind.Graphic (Kind, (~>), row, star)
import Language.Wart.Scheme.Syntax (Scheme)
import Language.Wart.Type.Syntax
import Type.Nat

data instance Node Type v =
  Node
  {-# UNPACK #-} !Int
  (Type (v (Node Type v)))
  (v BindingFlag)
  (v (Binder Type v))
  (v (Node Kind v)) deriving Generic
instance HasLabel (Node Type)
instance HasTerm (Node Type)
instance Functor f => HasBindingFlag (->) f (Node Type v) (v BindingFlag)
instance Functor f => HasBinder (->) f (Node Type v) (v (Binder Type v))

data instance Binder Type v
  = Scheme (Node Scheme v)
  | Type (v (Node Type v)) deriving Generic
instance (Choice p, Applicative f) => AsScheme p f (Binder Type)
instance AsType (Binder Type)

kind :: Lens' (Node Type v) (v (Node Kind v))
kind = Tuple.ix (Proxy :: Proxy N4)

bot :: (MonadSupply Int m, MonadUnionFind v m)
    => ReaderT (BindingFlag, Binder Kind v) m (v (Node Kind v))
    -> ReaderT (BindingFlag, Binder Type v) m (v (Node Type v))
bot = type' Bot

const' :: (MonadSupply Int m, MonadUnionFind v m)
       => Const
       -> ReaderT (BindingFlag, Binder Kind v) m (v (Node Kind v))
       -> ReaderT (BindingFlag, Binder Type v) m (v (Node Type v))
const' = type' . Const

app :: (MonadSupply Int m, MonadUnionFind v m)
    => ReaderT (BindingFlag, Binder Type v) m (v (Node Type v))
    -> ReaderT (BindingFlag, Binder Type v) m (v (Node Type v))
    -> ReaderT (BindingFlag, Binder Kind v) m (v (Node Kind v))
    -> ReaderT (BindingFlag, Binder Type v) m (v (Node Type v))
app m_f m_a = type' $ App m_f m_a

extend :: (MonadSupply Int m, MonadUnionFind v m)
       => Label -> ReaderT (BindingFlag, Binder Type v) m (v (Node Type v))
extend l = const' (Extend l) (star ~> row ~> row)

type' :: (MonadSupply Int m, MonadUnionFind v m)
      => Type (ReaderT (BindingFlag, Binder Type v) m (v (Node Type v)))
      -> ReaderT (BindingFlag, Binder Kind v) m (v (Node Kind v))
      -> ReaderT (BindingFlag, Binder Type v) m (v (Node Type v))
type' c m_k =
  new =<<
  Node <$>
  supply <*>
  sequenceA c <*>
  (new =<< view _1) <*>
  (new =<< view _2) <*>
  withReaderT (_2 %~ cloneBinder) m_k

#ifndef HLINT
cloneBinder :: (AsScheme Tagged Identity s, AsType s) => Binder Type v -> s v
cloneBinder = \ case
  Scheme n_s -> _Scheme#n_s
  Type v_t -> _Type#v_t
#endif
